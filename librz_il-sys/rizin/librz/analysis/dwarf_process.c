// SPDX-FileCopyrightText: 2012-2020 houndthe <cgkajm@gmail.com>
// SPDX-License-Identifier: LGPL-3.0-only

#include <rz_util.h>
#include <rz_type.h>
#include <sdb.h>
#include <rz_analysis.h>
#include <rz_bin_dwarf.h>
#include <string.h>

typedef struct dwarf_parse_context_t {
	const RzAnalysis *analysis;
	const RzBinDwarfDie *all_dies;
	const ut64 count;
	Sdb *sdb;
	HtUP /*<ut64 offset, DwarfDie *die>*/ *die_map;
	HtUP /*<offset, RzBinDwarfLocList*>*/ *locations;
	char *lang; // for demangling
} Context;

typedef struct dwarf_function_t {
	ut64 addr;
	const char *name;
	const char *signature;
	bool is_external;
	bool is_method;
	bool is_virtual;
	bool is_trampoline; // intermediary in making call to another func
	ut8 access; // public = 1, protected = 2, private = 3, if not set assume private
	ut64 vtable_addr; // location description
	ut64 call_conv; // normal || program || nocall
} Function;

typedef enum dwarf_location_kind {
	LOCATION_UNKNOWN = 0,
	LOCATION_GLOBAL = 1,
	LOCATION_BP = 2,
	LOCATION_SP = 3,
	LOCATION_REGISTER = 4,
	LOCATION_CFA = 5
} VariableLocationKind;
typedef struct dwarf_var_location_t {
	VariableLocationKind kind;
	ut64 address;
	ut64 reg_num;
	st64 offset;
	const char *reg_name; /* string literal */
} VariableLocation;

typedef struct dwarf_variable_t {
	VariableLocation *location;
	char *name;
	char *type;
} Variable;

static void variable_free(Variable *var) {
	free(var->name);
	free(var->location);
	free(var->type);
	free(var);
}

/* return -1 if attr isn't found */
static inline st32 find_attr_idx(const RzBinDwarfDie *die, st32 attr_name) {
	st32 i;
	rz_return_val_if_fail(die, -1);
	for (i = 0; i < die->count; i++) {
		if (die->attr_values[i].attr_name == attr_name) {
			return i;
		}
	}
	return -1;
}

/* return NULL if attr isn't found */
static RzBinDwarfAttrValue *find_attr(const RzBinDwarfDie *die, st32 attr_name) {
	st32 i;
	rz_return_val_if_fail(die, NULL);
	for (i = 0; i < die->count; i++) {
		if (die->attr_values[i].attr_name == attr_name) {
			return &die->attr_values[i];
		}
	}
	return NULL;
}

static inline char *create_type_name_from_offset(ut64 offset) {
	return rz_str_newf("type_0x%" PFMT64x, offset);
}

/**
 * \brief Get the DIE name or create unique one from its offset
 *
 * \param die
 * \return char* DIEs name or NULL if error
 */
static char *get_die_name(const RzBinDwarfDie *die) {
	char *name = NULL;
	st32 name_attr_idx = find_attr_idx(die, DW_AT_name);
	if (name_attr_idx != -1) {
		const char *s = rz_bin_dwarf_attr_value_get_string_content(&die->attr_values[name_attr_idx]);
		name = RZ_STR_DUP(s);
	}
	return name ? name : create_type_name_from_offset(die->offset);
}

/**
 * \brief Get the DIE size in bits
 *
 * \param die
 * \return ut64 size in bits or 0 if not found
 */
static ut64 get_die_size(const RzBinDwarfDie *die) {
	ut64 size = 0;
	st32 byte_size_idx = find_attr_idx(die, DW_AT_byte_size);

	if (byte_size_idx != -1) {
		size = die->attr_values[byte_size_idx].uconstant * CHAR_BIT;
	} else {
		st32 bit_size_idx = find_attr_idx(die, DW_AT_bit_size);

		if (bit_size_idx != -1) {
			size = die->attr_values[bit_size_idx].uconstant;
		}
	}
	return size;
}

/**
 * \brief Parse and return the count of an array or 0 if not found/not defined
 */
static ut64 parse_array_count(Context *ctx, ut64 idx) {
	const RzBinDwarfDie *die = &ctx->all_dies[idx++];

	if (die->has_children) {
		int child_depth = 1;
		size_t j;
		for (j = idx; child_depth > 0 && j < ctx->count; j++) {
			const RzBinDwarfDie *child_die = &ctx->all_dies[j];
			// right now we skip non direct descendats of the structure
			// can be also DW_TAG_suprogram for class methods or tag for templates
			if (child_depth == 1 && child_die->tag == DW_TAG_subrange_type) {
				size_t i;
				for (i = 0; i < child_die->count; i++) {
					const RzBinDwarfAttrValue *value = &child_die->attr_values[i];
					switch (value->attr_name) {
					case DW_AT_upper_bound:
					case DW_AT_count:
						return value->uconstant + 1;
						break;
					default:
						break;
					}
				}
			}
			if (child_die->has_children) {
				child_depth++;
			}
			// sibling list is terminated by null entry
			if (child_die->abbrev_code == 0) {
				child_depth--;
			}
		}
	}
	return 0;
}

static RzType *parse_type(Context *ctx, const ut64 offset, RZ_NULLABLE ut64 *size, RZ_NONNULL SetU *visited);

/**
 * Parse the die's DW_AT_type type or return a void type or NULL if \p type_idx == -1
 *
 * \param allow_void whether to return a void type instead of NULL if there is no type defined
 */
static RzType *parse_type_in_die(Context *ctx, RzBinDwarfDie *die, bool allow_void, RZ_NULLABLE ut64 *size, RZ_NONNULL SetU *visited) {
	st32 type_idx = find_attr_idx(die, DW_AT_type);
	if (type_idx == -1) {
		if (allow_void) {
			return rz_type_identifier_of_base_type_str(ctx->analysis->typedb, "void");
		}
		return NULL;
	}
	return parse_type(ctx, die->attr_values[type_idx].reference, size, visited);
}

/**
 * \brief Recursively parses type entry of a certain offset and saves type size into *size
 *
 * \param ctx
 * \param offset offset of the type entry
 * \param size_out ptr to size of a type to fill up (can be NULL if unwanted)
 * \param set of visited die offsets, to prevent infinite recursion
 * \return the parsed RzType or NULL on failure
 */
static RzType *parse_type(Context *ctx, const ut64 offset, RZ_NULLABLE ut64 *size, RZ_NONNULL SetU *visited) {
	rz_return_val_if_fail(visited, NULL);
	if (set_u_contains(visited, offset)) {
		return NULL;
	}
	RzBinDwarfDie *die = ht_up_find(ctx->die_map, offset, NULL);
	if (!die) {
		return NULL;
	}

	set_u_add(visited, offset);
	RzType *ret = NULL;
	// get size of first type DIE that has size
	if (size && *size == 0) {
		*size = get_die_size(die);
	}
	switch (die->tag) {
	// this should be recursive search for the type until you find base/user defined type
	case DW_TAG_pointer_type:
	case DW_TAG_reference_type: // C++ references are just pointers to us
	case DW_TAG_rvalue_reference_type: {
		RzType *pointee = parse_type_in_die(ctx, die, true, size, visited);
		if (!pointee) {
			goto end;
		}
		ret = rz_type_pointer_of_type(ctx->analysis->typedb, pointee, false);
		if (!ret) {
			rz_type_free(pointee);
		}
		break;
	}
	// We won't parse them as a complete type, because that will already be done
	// so just a name now
	case DW_TAG_typedef:
	case DW_TAG_base_type:
	case DW_TAG_structure_type:
	case DW_TAG_enumeration_type:
	case DW_TAG_union_type:
	case DW_TAG_class_type: {
		char *name = get_die_name(die);
		if (!name) {
			goto end;
		}
		ret = RZ_NEW0(RzType);
		if (!ret) {
			free(name);
			goto end;
		}
		ret->kind = RZ_TYPE_KIND_IDENTIFIER;
		ret->identifier.name = name;
		switch (die->tag) {
		case DW_TAG_structure_type:
			ret->identifier.kind = RZ_TYPE_IDENTIFIER_KIND_STRUCT;
			break;
		case DW_TAG_union_type:
			ret->identifier.kind = RZ_TYPE_IDENTIFIER_KIND_UNION;
			break;
		case DW_TAG_enumeration_type:
			ret->identifier.kind = RZ_TYPE_IDENTIFIER_KIND_ENUM;
			break;
		}
		break;
	}
	case DW_TAG_subroutine_type: {
		RzType *return_type = parse_type_in_die(ctx, die, true, size, visited);
		if (!return_type) {
			goto end;
		}
		if (die->has_children) { // has parameters
			// TODO
		}
		RzCallable *callable = rz_type_callable_new(NULL);
		if (!callable) {
			rz_type_free(return_type);
			goto end;
		}
		ret = rz_type_callable(callable);
		if (!ret) {
			rz_type_callable_free(callable);
		}
		break;
	}
	case DW_TAG_array_type: {
		RzType *subtype = parse_type_in_die(ctx, die, false, size, visited);
		if (!subtype) {
			goto end;
		}
		ut64 count = parse_array_count(ctx, die - ctx->all_dies);
		ret = rz_type_array_of_type(ctx->analysis->typedb, subtype, count);
		if (!ret) {
			rz_type_free(subtype);
		}
		break;
	}
	case DW_TAG_const_type: {
		ret = parse_type_in_die(ctx, die, false, size, visited);
		if (ret) {
			switch (ret->kind) {
			case RZ_TYPE_KIND_IDENTIFIER:
				ret->identifier.is_const = true;
				break;
			case RZ_TYPE_KIND_POINTER:
				ret->pointer.is_const = true;
				break;
			default:
				// const not supported yet for other kinds
				break;
			}
		}
		break;
	}
	case DW_TAG_volatile_type:
	case DW_TAG_restrict_type:
		// volatile and restrict attributes not supported in RzType
		ret = parse_type_in_die(ctx, die, false, size, visited);
		break;
	default:
		break;
	}
end:
	set_u_delete(visited, offset);
	return ret;
}

/**
 * \brief Convenience function for calling parse_type with an empty visited set
 * See documentation of parse_type
 */
static RzType *parse_type_outer(Context *ctx, const ut64 offset, ut64 *size) {
	SetU *visited = set_u_new();
	if (!visited) {
		return NULL;
	}
	RzType *r = parse_type(ctx, offset, size, visited);
	set_u_free(visited);
	return r;
}

/**
 * \brief Parses structured entry into *result RzTypeStructMember
 * http://www.dwarfstd.org/doc/DWARF4.pdf#page=102&zoom=100,0,0
 *
 * \param ctx
 * \param idx index of the current entry
 * \param result ptr to result member to fill up
 * \return RzTypeStructMember* ptr to parsed Member
 */
static RzTypeStructMember *parse_struct_member(Context *ctx, ut64 idx, RzTypeStructMember *result) {
	rz_return_val_if_fail(result, NULL);
	const RzBinDwarfDie *die = &ctx->all_dies[idx];

	char *name = NULL;
	RzType *type = NULL;
	ut64 offset = 0;
	ut64 size = 0;
	size_t i;
	for (i = 0; i < die->count; i++) {
		RzBinDwarfAttrValue *value = &die->attr_values[i];
		switch (die->attr_values[i].attr_name) {
		case DW_AT_name:
			free(name);
			name = get_die_name(die);
			if (!name) {
				goto cleanup;
			}
			break;
		case DW_AT_type:
			rz_type_free(type);
			type = parse_type_outer(ctx, value->reference, &size);
			break;
		case DW_AT_data_member_location:
			/*
				2 cases, 1.: If val is integer, it offset in bytes from
				the beginning of containing entity. If containing entity has
				a bit offset, member has that bit offset aswell
				2.: value is a location description
				http://www.dwarfstd.org/doc/DWARF4.pdf#page=39&zoom=100,0,0
			*/
			offset = value->uconstant;
			break;
		case DW_AT_accessibility: // private, public etc.
		case DW_AT_mutable: // flag is it is mutable
		case DW_AT_data_bit_offset:
			/*
				int that specifies the number of bits from beginning
				of containing entity to the beginning of the data member
			*/
			break;
		// If the size of a data member is not the same as the
		//  size of the type given for the data member
		case DW_AT_byte_size:
			size = value->uconstant * CHAR_BIT;
			break;
		case DW_AT_bit_size:
			size = value->uconstant;
			break;
		case DW_AT_containing_type:
		default:
			break;
		}
	}
	if (!type) {
		goto cleanup;
	}
	result->name = name;
	result->type = type;
	result->offset = offset;
	result->size = size;
	return result;

cleanup:
	free(name);
	rz_type_free(type);
	return NULL;
}

/**
 * \brief  Parses enum entry into *result RzTypeEnumCase
 * http://www.dwarfstd.org/doc/DWARF4.pdf#page=110&zoom=100,0,0
 *
 * \param ctx
 * \param idx index of the current entry
 * \param result ptr to result case to fill up
 * \return RzTypeEnumCase* Ptr to parsed enum case
 */
static RzTypeEnumCase *parse_enumerator(Context *ctx, ut64 idx, RzTypeEnumCase *result) {
	const RzBinDwarfDie *die = &ctx->all_dies[idx];

	char *name = NULL;
	int val = 0;
	size_t i;

	// Enumerator has DW_AT_name and DW_AT_const_value
	for (i = 0; i < die->count; i++) {
		RzBinDwarfAttrValue *value = &die->attr_values[i];
		switch (die->attr_values[i].attr_name) {
		case DW_AT_name:
			free(name);
			name = get_die_name(die);
			if (!name) {
				goto cleanup;
			}
			break;
		case DW_AT_const_value:
			// ?? can be block, sdata, data, string w/e
			val = value->uconstant; // TODO solve the encoding, I don't know in which union member is it store
			break;
		default:
			break;
		}
	}

	result->name = name;
	result->val = (int)val;
	return result;
cleanup:
	free(name);
	return NULL;
}

/**
 * \brief  Parses a structured entry (structs, classes, unions) into
 *         RzBaseType and saves it using rz_analysis_save_base_type ()
 *
 * \param ctx
 * \param idx index of the current entry
 */
// http://www.dwarfstd.org/doc/DWARF4.pdf#page=102&zoom=100,0,0
static void parse_structure_type(Context *ctx, ut64 idx) {
	const RzBinDwarfDie *die = &ctx->all_dies[idx];

	RzBaseTypeKind kind;
	if (die->tag == DW_TAG_union_type) {
		kind = RZ_BASE_TYPE_KIND_UNION;
	} else {
		kind = RZ_BASE_TYPE_KIND_STRUCT;
	}

	RzBaseType *base_type = rz_type_base_type_new(kind);
	if (!base_type) {
		return;
	}

	base_type->name = get_die_name(die);
	if (!base_type->name) {
		rz_type_base_type_free(base_type);
		return;
	}

	// if it is definition of previous declaration (TODO Fix, big ugly hotfix addition)
	st32 spec_attr_idx = find_attr_idx(die, DW_AT_specification);
	if (spec_attr_idx != -1) {
		RzBinDwarfDie *decl_die = ht_up_find(ctx->die_map, die->attr_values[spec_attr_idx].reference, NULL);
		if (!decl_die) {
			rz_type_base_type_free(base_type);
			return;
		}
		st32 name_attr_idx = find_attr_idx(decl_die, DW_AT_name);
		if (name_attr_idx != -1) {
			free(base_type->name);
			base_type->name = get_die_name(decl_die);
		}
	}

	base_type->size = get_die_size(die);

	RzTypeStructMember member = { 0 };
	// Parse out all members, can this in someway be extracted to a function?
	if (die->has_children) {
		int child_depth = 1; // Direct children of the node
		size_t j;
		idx++; // Move to the first children node
		for (j = idx; child_depth > 0 && j < ctx->count; j++) {
			const RzBinDwarfDie *child_die = &ctx->all_dies[j];
			// we take only direct descendats of the structure
			// can be also DW_TAG_suprogram for class methods or tag for templates
			if (child_depth == 1 && child_die->tag == DW_TAG_member) {
				RzTypeStructMember *result = parse_struct_member(ctx, j, &member);
				if (!result) {
					rz_type_base_type_free(base_type);
					return;
				} else {
					void *element = rz_vector_push(&base_type->struct_data.members, &member);
					if (!element) {
						rz_type_base_type_free(base_type);
						return;
					}
				}
			}
			if (child_die->has_children) {
				child_depth++;
			}
			if (child_die->abbrev_code == 0) { // siblings terminator
				child_depth--;
			}
		}
	}
	rz_type_db_save_base_type(ctx->analysis->typedb, base_type);
}

/**
 * \brief Parses a enum entry into RzBaseType and saves it
 *        int Sdb using rz_analysis_save_base_type ()
 *
 * \param ctx
 * \param idx index of the current entry
 */
static void parse_enum_type(Context *ctx, ut64 idx) {
	const RzBinDwarfDie *die = &ctx->all_dies[idx];

	RzBaseType *base_type = rz_type_base_type_new(RZ_BASE_TYPE_KIND_ENUM);
	if (!base_type) {
		return;
	}

	base_type->name = get_die_name(die);
	if (!base_type->name) {
		rz_type_base_type_free(base_type);
		return;
	}
	base_type->size = get_die_size(die);

	st32 type_attr_idx = find_attr_idx(die, DW_AT_type);
	if (type_attr_idx != -1) {
		base_type->type = parse_type_outer(ctx, die->attr_values[type_attr_idx].reference, &base_type->size);
		if (!base_type->type) {
			rz_type_base_type_free(base_type);
			return;
		}
	}

	RzTypeEnumCase cas;
	if (die->has_children) {
		int child_depth = 1; // Direct children of the node
		size_t j;
		idx++; // Move to the first children node
		for (j = idx; child_depth > 0 && j < ctx->count; j++) {
			const RzBinDwarfDie *child_die = &ctx->all_dies[j];
			// we take only direct descendats of the structure
			if (child_depth == 1 && child_die->tag == DW_TAG_enumerator) {
				RzTypeEnumCase *result = parse_enumerator(ctx, j, &cas);
				if (!result) {
					rz_type_base_type_free(base_type);
					return;
				} else {
					void *element = rz_vector_push(&base_type->enum_data.cases, &cas);
					if (!element) {
						rz_type_base_enum_case_free(result, NULL);
						rz_type_base_type_free(base_type);
						return;
					}
				}
			}
			if (child_die->has_children) {
				child_depth++;
			}
			// sibling list is terminated by null entry
			if (child_die->abbrev_code == 0) {
				child_depth--;
			}
		}
	}
	rz_type_db_save_base_type(ctx->analysis->typedb, base_type);
}

/**
 * \brief Parses a typedef entry into RzBaseType and saves it
 *        using rz_analysis_save_base_type ()
 *
 * http://www.dwarfstd.org/doc/DWARF4.pdf#page=96&zoom=100,0,0
 *
 * \param ctx
 * \param idx index of the current entry
 */
static void parse_typedef(Context *ctx, ut64 idx) {
	const RzBinDwarfDie *die = &ctx->all_dies[idx];

	char *name = NULL;
	RzType *type = NULL;
	ut64 size = 0;
	size_t i;

	for (i = 0; i < die->count; i++) {
		RzBinDwarfAttrValue *value = &die->attr_values[i];
		switch (die->attr_values[i].attr_name) {
		case DW_AT_name:
			name = get_die_name(die);
			if (!name) {
				goto cleanup;
			}
			break;
		case DW_AT_type:
			rz_type_free(type);
			type = parse_type_outer(ctx, value->reference, &size);
			if (!type) {
				goto cleanup;
			}
			break;
		default:
			break;
		}
	}
	if (!name || !type) { // type has to have a name for now
		goto cleanup;
	}
	RzBaseType *base_type = rz_type_base_type_new(RZ_BASE_TYPE_KIND_TYPEDEF);
	if (!base_type) {
		goto cleanup;
	}
	base_type->name = name;
	base_type->type = type;
	rz_type_db_save_base_type(ctx->analysis->typedb, base_type);
	return;

cleanup:
	rz_type_free(type);
}

static void parse_atomic_type(Context *ctx, ut64 idx) {
	const RzBinDwarfDie *die = &ctx->all_dies[idx];

	char *name = NULL;
	ut64 size = 0;
	size_t i;
	// TODO support endiannity and encoding in future?
	for (i = 0; i < die->count; i++) {
		RzBinDwarfAttrValue *value = &die->attr_values[i];
		switch (die->attr_values[i].attr_name) {
		case DW_AT_name: {
			free(name);
			const char *s = rz_bin_dwarf_attr_value_get_string_content(&die->attr_values[i]);
			if (s) {
				name = strdup(s);
			} else {
				name = create_type_name_from_offset(die->offset);
			}
			if (!name) {
				return;
			}
			break;
		}
		case DW_AT_byte_size:
			size = value->uconstant * CHAR_BIT;
			break;
		case DW_AT_bit_size:
			size = value->uconstant;
			break;
		case DW_AT_encoding:
		default:
			break;
		}
	}
	if (!name) { // type has to have a name for now
		return;
	}
	RzBaseType *base_type = rz_type_base_type_new(RZ_BASE_TYPE_KIND_ATOMIC);
	if (!base_type) {
		free(name);
		return;
	}
	base_type->name = name;
	base_type->size = size;
	rz_type_db_save_base_type(ctx->analysis->typedb, base_type);
}

static const char *get_specification_die_name(const RzBinDwarfDie *die) {
	st32 linkage_name_attr_idx = find_attr_idx(die, DW_AT_linkage_name);
	if (linkage_name_attr_idx != -1) {
		const char *s = rz_bin_dwarf_attr_value_get_string_content(&die->attr_values[linkage_name_attr_idx]);
		if (s) {
			return s;
		}
	}
	st32 name_attr_idx = find_attr_idx(die, DW_AT_name);
	if (name_attr_idx != -1) {
		const char *s = rz_bin_dwarf_attr_value_get_string_content(&die->attr_values[name_attr_idx]);
		if (s) {
			return s;
		}
	}
	return NULL;
}

static RzType *get_spec_die_type(Context *ctx, RzBinDwarfDie *die) {
	st32 attr_idx = find_attr_idx(die, DW_AT_type);
	if (attr_idx != -1) {
		ut64 size = 0;
		return parse_type_outer(ctx, die->attr_values[attr_idx].reference, &size);
	}
	return NULL;
}

/* For some languages linkage name is more informative like C++,
   but for Rust it's rubbish and the normal name is fine */
static bool prefer_linkage_name(char *lang) {
	if (!lang) {
		return false;
	}
	if (!strcmp(lang, "rust")) {
		return false;
	} else if (!strcmp(lang, "ada")) {
		return false;
	}
	return true;
}

static RzType *parse_abstract_origin(Context *ctx, ut64 offset, const char **name) {
	RzBinDwarfDie *die = ht_up_find(ctx->die_map, offset, NULL);
	if (die) {
		size_t i;
		ut64 size = 0;
		bool has_linkage_name = false;
		bool get_linkage_name = prefer_linkage_name(ctx->lang);
		for (i = 0; i < die->count; i++) {
			const RzBinDwarfAttrValue *val = &die->attr_values[i];
			switch (val->attr_name) {
			case DW_AT_name:
				if ((!get_linkage_name || !has_linkage_name) && val->kind == DW_AT_KIND_STRING) {
					*name = val->string.content;
				}
				break;
			case DW_AT_linkage_name:
			case DW_AT_MIPS_linkage_name:
				if (val->kind == DW_AT_KIND_STRING) {
					*name = val->string.content;
					has_linkage_name = true;
				}
				break;
			case DW_AT_type:
				return parse_type_outer(ctx, val->reference, &size);
				break;
			default:
				break;
			}
		}
	}
	return NULL;
}

/* x86_64 https://software.intel.com/sites/default/files/article/402129/mpx-linux64-abi.pdf */
static const char *map_dwarf_reg_to_x86_64_reg(ut64 reg_num, VariableLocationKind *kind) {
	*kind = LOCATION_REGISTER;
	switch (reg_num) {
	case 0: return "rax";
	case 1: return "rdx";
	case 2: return "rcx";
	case 3: return "rbx";
	case 4: return "rsi";
	case 5: return "rdi";
	case 6:
		*kind = LOCATION_BP;
		return "rbp";
	case 7:
		*kind = LOCATION_SP;
		return "rsp";
	case 8: return "r8";
	case 9: return "r9";
	case 10: return "r10";
	case 11: return "r11";
	case 12: return "r12";
	case 13: return "r13";
	case 14: return "r14";
	case 15: return "r15";
	case 17: return "xmm0";
	case 18: return "xmm1";
	case 19: return "xmm2";
	case 20: return "xmm3";
	case 21: return "xmm4";
	case 22: return "xmm5";
	case 23: return "xmm6";
	case 24: return "xmm7";
	default:
		*kind = LOCATION_UNKNOWN;
		return "unsupported_reg";
	}
}

/* x86 https://01.org/sites/default/files/file_attach/intel386-psabi-1.0.pdf */
static const char *map_dwarf_reg_to_x86_reg(ut64 reg_num, VariableLocationKind *kind) {
	*kind = LOCATION_REGISTER;
	switch (reg_num) {
	case 0:
	case 8:
		return "eax";
	case 1: return "edx";
	case 2: return "ecx";
	case 3: return "ebx";
	case 4:
		*kind = LOCATION_SP;
		return "esp";
	case 5:
		*kind = LOCATION_BP;
		return "ebp";
	case 6: return "esi";
	case 7: return "edi";
	case 9: return "EFLAGS";
	case 11: return "st0";
	case 12: return "st1";
	case 13: return "st2";
	case 14: return "st3";
	case 15: return "st4";
	case 16: return "st5";
	case 17: return "st6";
	case 18: return "st7";
	case 21: return "xmm0";
	case 22: return "xmm1";
	case 23: return "xmm2";
	case 24: return "xmm3";
	case 25: return "xmm4";
	case 26: return "xmm5";
	case 27: return "xmm6";
	case 28: return "xmm7";
	case 29: return "mm0";
	case 30: return "mm1";
	case 31: return "mm2";
	case 32: return "mm3";
	case 33: return "mm4";
	case 34: return "mm5";
	case 35: return "mm6";
	case 36: return "mm7";
	case 40: return "es";
	case 41: return "cs";
	case 42: return "ss";
	case 43: return "ds";
	case 44: return "fs";
	case 45: return "gs";
	default:
		rz_warn_if_reached();
		*kind = LOCATION_UNKNOWN;
		return "unsupported_reg";
	}
}

/* https://refspecs.linuxfoundation.org/ELF/ppc64/PPC-elf64abi-1.9.html#DW-REG */
static const char *map_dwarf_reg_to_ppc64_reg(ut64 reg_num, VariableLocationKind *kind) {
	*kind = LOCATION_REGISTER;
	switch (reg_num) {
	case 0: return "r0";
	case 1:
		*kind = LOCATION_SP;
		return "r1";
	case 2: return "r2";
	case 3: return "r3";
	case 4: return "r4";
	case 5: return "r5";
	case 6: return "r6";
	case 7: return "r7";
	case 8: return "r8";
	case 9: return "r9";
	case 10: return "r10";
	case 11: return "r11";
	case 12: return "r12";
	case 13: return "r13";
	case 14: return "r14";
	case 15: return "r15";
	case 16: return "r16";
	case 17: return "r17";
	case 18: return "r18";
	case 19: return "r19";
	case 20: return "r20";
	case 21: return "r21";
	case 22: return "r22";
	case 23: return "r23";
	case 24: return "r24";
	case 25: return "r25";
	case 26: return "r26";
	case 27: return "r27";
	case 28: return "r28";
	case 29: return "r29";
	case 30: return "r30";
	case 31: return "r31";
	default:
		rz_warn_if_reached();
		*kind = LOCATION_UNKNOWN;
		return "unsupported_reg";
	}
}

/* returns string literal register name!
   TODO add more arches                 */
static const char *get_dwarf_reg_name(RZ_NONNULL char *arch, int reg_num, VariableLocationKind *kind, int bits) {
	if (!strcmp(arch, "x86")) {
		if (bits == 64) {
			return map_dwarf_reg_to_x86_64_reg(reg_num, kind);
		} else {
			return map_dwarf_reg_to_x86_reg(reg_num, kind);
		}
	} else if (!strcmp(arch, "ppc")) {
		if (bits == 64) {
			return map_dwarf_reg_to_ppc64_reg(reg_num, kind);
		}
	}
	*kind = LOCATION_UNKNOWN;
	return "unsupported_reg";
}

static RzBinDwarfLocRange *find_largest_loc_range(RzList /*<RzBinDwarfLocRange *>*/ *loc_list) {
	RzBinDwarfLocRange *largest = NULL;
	ut64 max_range_size = 0;
	RzListIter *iter;
	RzBinDwarfLocRange *range;
	rz_list_foreach (loc_list, iter, range) {
		ut64 diff = range->end - range->start;
		if (diff > max_range_size) {
			max_range_size = diff;
			largest = range;
		}
	}
	return largest;
}

/* TODO move a lot of the parsing here into dwarf.c and do only processing here */
static VariableLocation *parse_dwarf_location(Context *ctx, const RzBinDwarfAttrValue *loc, const RzBinDwarfAttrValue *frame_base) {
	/* reg5 - val is in register 5
	fbreg <leb> - offset from frame base
	regx <leb> - contents is in register X
	addr <addr> - contents is in at addr
	bregXX <leb> - contents is at offset from specified register
	- we now support 3 options: SP, BP and register based arguments */

	/* Loclist offset is usually CONSTANT or REFERENCE at older DWARF versions, new one has LocListPtr for that */
	if (loc->kind != DW_AT_KIND_BLOCK && loc->kind != DW_AT_KIND_LOCLISTPTR && loc->kind != DW_AT_KIND_REFERENCE && loc->kind != DW_AT_KIND_CONSTANT) {
		return NULL;
	}
	RzBinDwarfBlock block;
	if (loc->kind == DW_AT_KIND_LOCLISTPTR || loc->kind == DW_AT_KIND_REFERENCE || loc->kind == DW_AT_KIND_CONSTANT) {
		ut64 offset = loc->reference;
		RzBinDwarfLocList *range_list = ht_up_find(ctx->locations, offset, NULL);
		if (!range_list) { /* for some reason offset isn't there, wrong parsing or malformed dwarf */
			return NULL;
		}
		/* use the largest range as a variable */
		RzBinDwarfLocRange *range = find_largest_loc_range(range_list->list);
		if (!range) {
			return NULL;
		}
		/* Very rough and sloppy, refactor this hacked up stuff */
		block = *range->expression;
		// range->expression... etc
	} else {
		block = loc->block;
	}
	VariableLocationKind kind = LOCATION_UNKNOWN;
	st64 offset = 0;
	ut64 address = 0;
	ut64 reg_num = -1;
	const char *reg_name = NULL; /* literal */
	size_t i;
	for (i = 0; i < block.length; i++) {
		switch (block.data[i]) {
		case DW_OP_fbreg: {
			/* TODO sometimes CFA is referenced, but we don't parse that yet
		   just an offset involving framebase of a function*/
			if (i == block.length - 1) {
				return NULL;
			}
			const ut8 *dump = &block.data[++i];
			offset = rz_sleb128(&dump, &block.data[loc->block.length]);
			if (frame_base) {
				/* recursive parsing, but frame_base should be only one, but someone
				   could make malicious resource exhaustion attack, so a depth counter might be cool? */
				VariableLocation *location = parse_dwarf_location(ctx, frame_base, NULL);
				if (location) {
					location->offset += offset;
					return location;
				}
				return NULL;
			} else {
				/* Might happen if frame_base has a frame_base reference? I don't think it can tho */
				return NULL;
			}
		} break;
		case DW_OP_reg0:
		case DW_OP_reg1:
		case DW_OP_reg2:
		case DW_OP_reg3:
		case DW_OP_reg4:
		case DW_OP_reg5:
		case DW_OP_reg6:
		case DW_OP_reg7:
		case DW_OP_reg8:
		case DW_OP_reg9:
		case DW_OP_reg10:
		case DW_OP_reg11:
		case DW_OP_reg12:
		case DW_OP_reg13:
		case DW_OP_reg14:
		case DW_OP_reg15:
		case DW_OP_reg16:
		case DW_OP_reg17:
		case DW_OP_reg18:
		case DW_OP_reg19:
		case DW_OP_reg20:
		case DW_OP_reg21:
		case DW_OP_reg22:
		case DW_OP_reg23:
		case DW_OP_reg24:
		case DW_OP_reg25:
		case DW_OP_reg26:
		case DW_OP_reg27:
		case DW_OP_reg28:
		case DW_OP_reg29:
		case DW_OP_reg30:
		case DW_OP_reg31: {
			/* Will mostly be used for SP based arguments */
			/* TODO I need to find binaries that uses this so I can test it out*/
			reg_num = block.data[i] - DW_OP_reg0; // get the reg number
			reg_name = get_dwarf_reg_name(ctx->analysis->cpu, reg_num, &kind, ctx->analysis->bits);
		} break;
		case DW_OP_breg0:
		case DW_OP_breg1:
		case DW_OP_breg2:
		case DW_OP_breg3:
		case DW_OP_breg4:
		case DW_OP_breg5:
		case DW_OP_breg6:
		case DW_OP_breg7:
		case DW_OP_breg8:
		case DW_OP_breg9:
		case DW_OP_breg10:
		case DW_OP_breg11:
		case DW_OP_breg12:
		case DW_OP_breg13:
		case DW_OP_breg14:
		case DW_OP_breg15:
		case DW_OP_breg16:
		case DW_OP_breg17:
		case DW_OP_breg18:
		case DW_OP_breg19:
		case DW_OP_breg20:
		case DW_OP_breg21:
		case DW_OP_breg22:
		case DW_OP_breg23:
		case DW_OP_breg24:
		case DW_OP_breg25:
		case DW_OP_breg26:
		case DW_OP_breg27:
		case DW_OP_breg28:
		case DW_OP_breg29:
		case DW_OP_breg30:
		case DW_OP_breg31: {
			if (i == block.length - 1) {
				return NULL;
			}
			/* The single operand of the DW_OP_bregn operations provides
			signed LEB128 offset from the specified register.  */
			reg_num = block.data[i] - DW_OP_breg0; // get the reg number
			const ut8 *buffer = &block.data[++i];
			offset = rz_sleb128(&buffer, &block.data[block.length]);
			/* TODO do a proper expression parsing, move by the amount of bytes sleb reads */
			i += buffer - &block.data[0];
			reg_name = get_dwarf_reg_name(ctx->analysis->cpu, reg_num, &kind, ctx->analysis->bits);
		} break;
		case DW_OP_bregx: {
			if (i == block.length - 1) {
				return NULL;
			}
			/* 2 operands, reg_number, offset*/
			/* I need to find binaries that uses this so I can test it out*/
			const ut8 *buffer = &block.data[++i];
			const ut8 *buf_end = &block.data[block.length];
			buffer = rz_uleb128(buffer, buf_end - buffer, &reg_num, NULL);
			if (buffer == buf_end) {
				return NULL;
			}
			offset = rz_sleb128(&buffer, buf_end);
			reg_name = get_dwarf_reg_name(ctx->analysis->cpu, reg_num, &kind, ctx->analysis->bits);
		} break;
		case DW_OP_addr: {
			/* The DW_OP_addr operation has a single operand that encodes a machine address and whose
			size is the size of an address on the target machine.  */
			const int addr_size = ctx->analysis->bits / 8;
			const ut8 *dump = &block.data[++i];
			/* malformed, not enough bytes to represent address */
			if (block.length - i < addr_size) {
				return NULL;
			}
			switch (addr_size) {
			case 1:
				address = rz_read_ble8(dump);
				break;
			case 2:
				address = rz_read_ble16(dump, ctx->analysis->big_endian);
				break;
			case 4:
				address = rz_read_ble32(dump, ctx->analysis->big_endian);
				break;
			case 8:
				address = rz_read_ble64(dump, ctx->analysis->big_endian);
				break;
			default:
				rz_warn_if_reached(); /* weird addr_size */
				return NULL;
			}
			kind = LOCATION_GLOBAL; // address
		} break;
		case DW_OP_call_frame_cfa: {
			// From the DWARF specs:
			//   The call frame is identified by an address on the stack. We refer to this address as the Canonical
			//   Frame Address or CFA. Typically, the CFA is defined to be the value of the stack
			//   pointer at the call site in the previous frame (which may be different from its value
			//   on entry to the current frame).
			// TODO: The following is only an educated guess. There is actually more involved in calculating the
			//       CFA correctly.
			kind = LOCATION_CFA;
			offset += ctx->analysis->bits / 8; // guessed return address size
		} break;
		default:
			break;
		}
	}
	if (kind == LOCATION_UNKNOWN) {
		return NULL;
	}
	VariableLocation *location = RZ_NEW0(VariableLocation);
	if (location) {
		location->reg_name = reg_name;
		location->reg_num = reg_num;
		location->kind = kind;
		location->offset = offset;
		location->address = address;
	}
	return location;
}

/**
 * Helper to temporarily serialize types into strings for legacy SDB storage.
 * Usages should be removed long-term.
 */
static RZ_DEPRECATE char *type_as_string(const RzTypeDB *typedb, RZ_NONNULL const RzType *type) {
	return rz_type_as_pretty_string(typedb, type, NULL,
		RZ_TYPE_PRINT_ZERO_VLA | RZ_TYPE_PRINT_NO_END_SEMICOLON | RZ_TYPE_PRINT_ANONYMOUS | RZ_TYPE_PRINT_ALLOW_NON_EXISTENT_BASE_TYPE, 0);
}

static st32 parse_function_args_and_vars(Context *ctx, ut64 idx, RzStrBuf *args, RzList /*<Variable *>*/ *variables) {
	const RzBinDwarfDie *die = &ctx->all_dies[idx++];

	if (die->has_children) {
		int child_depth = 1;

		bool get_linkage_name = prefer_linkage_name(ctx->lang);
		bool has_linkage_name = false;
		int argNumber = 1;
		size_t j;
		for (j = idx; child_depth > 0 && j < ctx->count; j++) {
			const RzBinDwarfDie *child_die = &ctx->all_dies[j];
			const char *name = NULL;
			if (child_die->tag == DW_TAG_formal_parameter || child_die->tag == DW_TAG_variable) {
				Variable *var = RZ_NEW0(Variable);
				RzType *type = NULL;
				size_t i;
				for (i = 0; i < child_die->count; i++) {
					const RzBinDwarfAttrValue *val = &child_die->attr_values[i];
					switch (val->attr_name) {
					case DW_AT_name:
						if ((!get_linkage_name || !has_linkage_name) && val->kind == DW_AT_KIND_STRING) {
							name = val->string.content;
						}
						break;
					case DW_AT_linkage_name:
					case DW_AT_MIPS_linkage_name:
						if (val->kind == DW_AT_KIND_STRING) {
							name = val->string.content;
						}
						has_linkage_name = true;
						break;
					case DW_AT_type:
						rz_type_free(type);
						type = parse_type_outer(ctx, val->reference, NULL);
						break;
					// abstract origin is supposed to have omitted information
					case DW_AT_abstract_origin:
						rz_type_free(type);
						type = parse_abstract_origin(ctx, val->reference, &name);
						break;
					case DW_AT_location:
						var->location = parse_dwarf_location(ctx, val, find_attr(die, DW_AT_frame_base));
						break;
					default:
						break;
					}
				}
				if (child_die->tag == DW_TAG_formal_parameter && child_depth == 1) {
					/* arguments sometimes have only type, create generic argX */
					if (type) {
						if (!name) {
							var->name = rz_str_newf("arg%d", argNumber);
						} else {
							var->name = strdup(name);
						}
						char *type_str = type_as_string(ctx->analysis->typedb, type);
						rz_strbuf_appendf(args, "%s %s,", rz_str_get(type_str), var->name);
						var->type = type_str;
						rz_list_append(variables, var);
					} else {
						variable_free(var);
					}
					argNumber++;
				} else { /* DW_TAG_variable */
					if (name && type) {
						var->name = strdup(name);
						var->type = type_as_string(ctx->analysis->typedb, type);
						rz_list_append(variables, var);
					} else {
						variable_free(var);
					}
				}
				rz_type_free(type);
			} else if (child_depth == 1 && child_die->tag == DW_TAG_unspecified_parameters) {
				rz_strbuf_appendf(args, "va_args ...,");
			}
			if (child_die->has_children) {
				child_depth++;
			}
			if (child_die->abbrev_code == 0) { /* sibling list is terminated by null entry */
				child_depth--;
			}
		}
		if (args->len > 0) {
			rz_strbuf_slice(args, 0, args->len - 1);
		}
	}
	return 0;
}

static void sdb_save_dwarf_function(Function *dwarf_fcn, RzList /*<Variable *>*/ *variables, Sdb *sdb) {
	char *sname = rz_str_sanitize_sdb_key(dwarf_fcn->name);
	sdb_set(sdb, sname, "fcn", 0);

	char *addr_key = rz_str_newf("fcn.%s.addr", sname);
	char *addr_val = rz_str_newf("0x%" PFMT64x "", dwarf_fcn->addr);
	sdb_set(sdb, addr_key, addr_val, 0);
	free(addr_key);
	free(addr_val);

	/* so we can have name without sanitization */
	char *name_key = rz_str_newf("fcn.%s.name", sname);
	char *name_val = rz_str_newf("%s", dwarf_fcn->name);
	sdb_set(sdb, name_key, name_val, 0);
	free(name_key);
	free(name_val);

	char *signature_key = rz_str_newf("fcn.%s.sig", sname);
	sdb_set(sdb, signature_key, dwarf_fcn->signature, 0);
	free(signature_key);

	RzStrBuf vars;
	rz_strbuf_init(&vars);
	RzListIter *iter;
	Variable *var;
	rz_list_foreach (variables, iter, var) {
		if (!var->location) {
			/* NULL location probably means optimized out, maybe put a comment there */
			continue;
		}
		char *key = NULL;
		char *val = NULL;
		switch (var->location->kind) {
		case LOCATION_BP:
		case LOCATION_CFA: {
			/* value = "type, storage, additional info based on storage (offset)" */

			rz_strbuf_appendf(&vars, "%s,", var->name);
			key = rz_str_newf("fcn.%s.var.%s", sname, var->name);
			val = rz_str_newf("%s,%" PFMT64d ",%s",
				var->location->kind == LOCATION_CFA ? "c" : "b",
				var->location->offset, var->type);
			sdb_set(sdb, key, val, 0);
		} break;
		case LOCATION_SP: {
			/* value = "type, storage, additional info based on storage (offset)" */

			rz_strbuf_appendf(&vars, "%s,", var->name);
			key = rz_str_newf("fcn.%s.var.%s", sname, var->name);
			val = rz_str_newf("%s,%" PFMT64d ",%s", "s", var->location->offset, var->type);
			sdb_set(sdb, key, val, 0);
		} break;
		case LOCATION_GLOBAL: {
			/* value = "type, storage, additional info based on storage (address)" */

			rz_strbuf_appendf(&vars, "%s,", var->name);
			key = rz_str_newf("fcn.%s.var.%s", sname, var->name);
			val = rz_str_newf("%s,%" PFMT64u ",%s", "g", var->location->address, var->type);
			sdb_set(sdb, key, val, 0);
		} break;
		case LOCATION_REGISTER: {
			/* value = "type, storage, additional info based on storage (register name)" */

			rz_strbuf_appendf(&vars, "%s,", var->name);
			key = rz_str_newf("fcn.%s.var.%s", sname, var->name);
			val = rz_str_newf("%s,%s,%s", "r", var->location->reg_name, var->type);
			sdb_set(sdb, key, val, 0);
		} break;

		default:
			/* else location is unknown (optimized out), skip the var */
			break;
		}
		free(key);
		free(val);
	}
	if (vars.len > 0) { /* remove the extra , */
		rz_strbuf_slice(&vars, 0, vars.len - 1); /* leaks? */
	}
	char *vars_key = rz_str_newf("fcn.%s.vars", sname);
	char *vars_val = rz_str_newf("%s", rz_strbuf_get(&vars));
	sdb_set(sdb, vars_key, vars_val, 0);
	free(vars_key);
	free(vars_val);
	rz_strbuf_fini(&vars);
	free(sname);
}

/**
 * \brief Parse function,it's arguments, variables and
 *        save the information into the Sdb
 *
 * \param ctx
 * \param idx Current entry index
 */
static void parse_function(Context *ctx, ut64 idx) {
	const RzBinDwarfDie *die = &ctx->all_dies[idx];

	Function fcn = { 0 };
	bool has_linkage_name = false;
	bool get_linkage_name = prefer_linkage_name(ctx->lang);
	RzType *ret_type = NULL;
	if (find_attr_idx(die, DW_AT_declaration) != -1) {
		return; /* just declaration skip */
	}
	size_t i;
	/* For rust binaries prefer regular name not linkage TODO */
	for (i = 0; i < die->count; i++) {
		RzBinDwarfAttrValue *val = &die->attr_values[i];
		switch (die->attr_values[i].attr_name) {
		case DW_AT_name:
			if (!get_linkage_name || !has_linkage_name) {
				fcn.name = val->kind == DW_AT_KIND_STRING ? val->string.content : fcn.name;
			}
			break;
		case DW_AT_linkage_name:
		case DW_AT_MIPS_linkage_name:
			fcn.name = val->kind == DW_AT_KIND_STRING ? val->string.content : fcn.name;
			has_linkage_name = true;
			break;
		case DW_AT_low_pc:
		case DW_AT_entry_pc:
			fcn.addr = val->kind == DW_AT_KIND_ADDRESS ? val->address : fcn.addr;
			break;
		case DW_AT_specification: /* reference to declaration DIE with more info */
		{
			RzBinDwarfDie *spec_die = ht_up_find(ctx->die_map, val->reference, NULL);
			if (spec_die) {
				fcn.name = get_specification_die_name(spec_die); /* I assume that if specification has a name, this DIE hasn't */
				rz_type_free(ret_type);
				ret_type = get_spec_die_type(ctx, spec_die);
			}
		} break;
		case DW_AT_type:
			rz_type_free(ret_type);
			ret_type = parse_type_outer(ctx, val->reference, NULL);
			break;
		case DW_AT_virtuality:
			fcn.is_method = true; /* method specific attr */
			fcn.is_virtual = true;
			break;
		case DW_AT_object_pointer:
			fcn.is_method = true;
			break;
		case DW_AT_vtable_elem_location:
			fcn.is_method = true;
			fcn.vtable_addr = 0; /* TODO we might use this information */
			break;
		case DW_AT_accessibility:
			fcn.is_method = true;
			fcn.access = (ut8)val->uconstant;
			break;
		case DW_AT_external:
			fcn.is_external = true;
			break;
		case DW_AT_trampoline:
			fcn.is_trampoline = true;
			break;
		case DW_AT_ranges:
		case DW_AT_high_pc:
		default:
			break;
		}
	}
	if (!fcn.name || !fcn.addr) { /* we need a name, faddr */
		goto cleanup;
	}
	RzStrBuf args;
	rz_strbuf_init(&args);
	/* TODO do the same for arguments in future so we can use their location */
	RzList /*<Variable*>*/ *variables = rz_list_new();
	parse_function_args_and_vars(ctx, idx, &args, variables);

	if (!ret_type) { /* DW_AT_type is omitted in case of `void` ret type */
		ret_type = rz_type_identifier_of_base_type_str(ctx->analysis->typedb, "void");
		if (!ret_type) {
			rz_list_free(variables);
			goto cleanup;
		}
	}
	rz_warn_if_fail(ctx->lang);
	char *new_name = ctx->analysis->binb.demangle(NULL, ctx->lang, fcn.name, fcn.addr, false);
	fcn.name = new_name ? new_name : strdup(fcn.name);
	char *ret_type_str = type_as_string(ctx->analysis->typedb, ret_type);
	fcn.signature = rz_str_newf("%s %s(%s);", rz_str_get(ret_type_str), fcn.name, rz_strbuf_get(&args));
	free(ret_type_str);
	sdb_save_dwarf_function(&fcn, variables, ctx->sdb);

	free((char *)fcn.signature);
	free((char *)fcn.name);

	RzListIter *iter;
	Variable *var;
	rz_list_foreach (variables, iter, var) {
		variable_free(var);
	}
	rz_list_free(variables);
	rz_strbuf_fini(&args);
cleanup:
	rz_type_free(ret_type);
}

/**
 * \brief Get's language from comp unit for demangling
 *
 * \param die
 * \return char* string literal language represantation for demangling BinDemangle
 */
static char *parse_comp_unit_lang(const RzBinDwarfDie *die) {
	rz_return_val_if_fail(die, NULL);

	int idx = find_attr_idx(die, DW_AT_language);
	char *lang = "cxx"; // default fallback
	if (idx == -1) {
		/* What to do now, it should have  one?, just assume C++ */
		return lang;
	}
	const RzBinDwarfAttrValue *val = &die->attr_values[idx];
	rz_warn_if_fail(val->kind == DW_AT_KIND_CONSTANT);

	switch (val->uconstant) {
	case DW_LANG_Java:
		return "java";
	case DW_LANG_ObjC:
	/* subideal, TODO research if dwarf gives me enough info to properly separate C++ and ObjC mangling */
	case DW_LANG_ObjC_plus_plus:
		return "objc";
	case DW_LANG_D:
		return "dlang";
	case DW_LANG_Rust:
		return "rust";
	case DW_LANG_C_plus_plus:
	case DW_LANG_C_plus_plus_14:
	/* no demangling available */
	case DW_LANG_Ada83:
	case DW_LANG_Cobol74:
	case DW_LANG_Cobol85:
	case DW_LANG_Fortran77:
	case DW_LANG_Fortran90:
	case DW_LANG_Pascal83:
	case DW_LANG_Modula2:
	case DW_LANG_Ada95:
	case DW_LANG_Fortran95:
	case DW_LANG_PLI:
	case DW_LANG_Python:
	case DW_LANG_Swift:
	case DW_LANG_Julia:
	case DW_LANG_Dylan:
	case DW_LANG_Fortran03:
	case DW_LANG_Fortran08:
	case DW_LANG_UPC:
	case DW_LANG_C:
	case DW_LANG_C89:
	case DW_LANG_C99:
	case DW_LANG_C11:
	default:
		return lang;
	}
	return lang;
}

/**
 * \brief Delegates DIE to it's proper parsing method
 *
 * \param ctx
 * \param idx index of the current entry
 */
static void parse_type_entry(Context *ctx, ut64 idx) {
	rz_return_if_fail(ctx);

	const RzBinDwarfDie *die = &ctx->all_dies[idx];
	switch (die->tag) {
	case DW_TAG_structure_type:
	case DW_TAG_union_type:
	case DW_TAG_class_type:
		parse_structure_type(ctx, idx);
		break;
	case DW_TAG_enumeration_type:
		parse_enum_type(ctx, idx);
		break;
	case DW_TAG_typedef:
		parse_typedef(ctx, idx);
		break;
	case DW_TAG_base_type:
		parse_atomic_type(ctx, idx);
		break;
	case DW_TAG_subprogram:
		parse_function(ctx, idx);
		break;
	case DW_TAG_compile_unit:
		/* used for name demangling */
		ctx->lang = parse_comp_unit_lang(die);
	default:
		break;
	}
}

/**
 * \brief Parses type and function information out of DWARF entries
 *        and stores them to the sdb for further use
 *
 * \param analysis
 * \param ctx
 */
RZ_API void rz_analysis_dwarf_process_info(const RzAnalysis *analysis, RzAnalysisDwarfContext *ctx) {
	rz_return_if_fail(ctx && analysis);
	Sdb *dwarf_sdb = sdb_ns(analysis->sdb, "dwarf", 1);
	size_t i, j;
	const RzBinDwarfDebugInfo *info = ctx->info;
	for (i = 0; i < info->count; i++) {
		RzBinDwarfCompUnit *unit = &info->comp_units[i];
		Context dw_context = { // context per unit?
			.analysis = analysis,
			.all_dies = unit->dies,
			.count = unit->count,
			.die_map = info->lookup_table,
			.sdb = dwarf_sdb,
			.locations = ctx->loc,
			.lang = NULL
		};
		for (j = 0; j < unit->count; j++) {
			parse_type_entry(&dw_context, j);
		}
	}
}

bool filter_sdb_function_names(void *user, const char *k, const char *v) {
	(void)user;
	(void)k;
	return !strcmp(v, "fcn");
}

/**
 * \brief Use parsed DWARF function info from Sdb in the function analysis
 *  XXX right now we only save parsed name and variables, we can't use signature now
 *  XXX refactor to be more readable
 * \param analysis
 * \param dwarf_sdb
 */
RZ_API void rz_analysis_dwarf_integrate_functions(RzAnalysis *analysis, RzFlag *flags, Sdb *dwarf_sdb) {
	rz_return_if_fail(analysis && dwarf_sdb);

	/* get all entries with value == func */
	SdbList *sdb_list = sdb_foreach_list_filter(dwarf_sdb, filter_sdb_function_names, false);
	SdbListIter *it;
	SdbKv *kv;
	/* iterate all function entries */
	ls_foreach (sdb_list, it, kv) {
		char *func_sname = kv->base.key;

		char *addr_key = rz_str_newf("fcn.%s.addr", func_sname);
		ut64 faddr = sdb_num_get(dwarf_sdb, addr_key, 0);
		free(addr_key);

		/* if the function is analyzed so we can edit */
		RzAnalysisFunction *fcn = rz_analysis_get_function_at(analysis, faddr);
		if (fcn) {
			/* prepend dwarf debug info stuff with dbg. */
			char *real_name_key = rz_str_newf("fcn.%s.name", func_sname);
			char *real_name = sdb_get(dwarf_sdb, real_name_key, 0);
			free(real_name_key);

			char *dwf_name = rz_str_newf("dbg.%s", real_name);
			free(real_name);

			rz_analysis_function_rename(fcn, dwf_name);
			free(dwf_name);

			char *tmp = rz_str_newf("fcn.%s.sig", func_sname);
			char *fcnstr = sdb_get(dwarf_sdb, tmp, 0);
			free(tmp);
			/* Apply signature as a comment at a function address */
			rz_meta_set_string(analysis, RZ_META_TYPE_COMMENT, faddr, fcnstr);
			free(fcnstr);
		}
		char *var_names_key = rz_str_newf("fcn.%s.vars", func_sname);
		char *vars = sdb_get(dwarf_sdb, var_names_key, NULL);
		char *var_name;
		sdb_aforeach(var_name, vars) {
			char *var_key = rz_str_newf("fcn.%s.var.%s", func_sname, var_name);
			char *var_data = sdb_get(dwarf_sdb, var_key, NULL);
			if (!var_data) {
				goto loop_end;
			}
			char *extra = NULL;
			char *kind = sdb_anext(var_data, &extra);
			char *type = NULL;
			extra = sdb_anext(extra, &type);
			if (!extra) {
				goto loop_end;
			}
			RzType *ttype = rz_type_parse_string_single(analysis->typedb->parser, type, NULL);
			if (!ttype) {
				goto loop_end;
			}
			st64 offset = 0;
			if (*kind != 'r') {
				offset = strtol(extra, NULL, 10);
			}
			if (*kind == 'g') { /* global, fixed addr TODO add size to variables? */
				char *global_name = rz_str_newf("global_%s", var_name);
				rz_flag_unset_off(flags, offset);
				rz_flag_set_next(flags, global_name, offset, 4);
				free(global_name);
			} else if (*kind == 'r' && fcn) {
				RzRegItem *i = rz_reg_get(analysis->reg, extra, -1);
				if (!i) {
					goto loop_end;
				}
				RzAnalysisVarStorage stor;
				rz_analysis_var_storage_init_reg(&stor, extra);
				rz_analysis_function_set_var(fcn, &stor, ttype, 4, var_name);
			} else if (fcn) { /* kind == 'b' || kind == 's' || kind == 'c' (stack variables) */
				RzAnalysisVarStorage stor;
				RzStackAddr addr = offset;
				if (*kind == 'b') {
					addr -= fcn->bp_off;
				}
				rz_analysis_var_storage_init_stack(&stor, addr);
				rz_analysis_function_set_var(fcn, &stor, ttype, 4, var_name);
			}
			rz_type_free(ttype);
			free(var_key);
			free(var_data);
		loop_end:
			sdb_aforeach_next(var_name);
		}
		free(var_names_key);
		free(vars);
	}
	ls_free(sdb_list);
}