//! Collection of functions from std.zig.ast that we need
//! and may hit undefined in the standard library implementation
//! when there are parser errors.

const std = @import("std");
const Ast = std.zig.Ast;
const Node = Ast.Node;
const full = Ast.full;
const builtin = @import("builtin");

fn fullPtrType(tree: Ast, info: full.PtrType.Components) full.PtrType {
    const token_tags = tree.tokens.items(.tag);
    // TODO: looks like stage1 isn't quite smart enough to handle enum
    // literals in some places here
    const Size = std.builtin.TypeInfo.Pointer.Size;
    const size: Size = switch (token_tags[info.main_token]) {
        .asterisk,
        .asterisk_asterisk,
        => switch (token_tags[info.main_token + 1]) {
            .r_bracket, .colon => .Many,
            .identifier => if (token_tags[info.main_token - 1] == .l_bracket) Size.C else .One,
            else => .One,
        },
        .l_bracket => Size.Slice,
        else => unreachable,
    };
    var result: full.PtrType = .{
        .size = size,
        .allowzero_token = null,
        .const_token = null,
        .volatile_token = null,
        .ast = info,
    };
    // We need to be careful that we don't iterate over any sub-expressions
    // here while looking for modifiers as that could result in false
    // positives. Therefore, start after a sentinel if there is one and
    // skip over any align node and bit range nodes.
    var i = if (info.sentinel != 0) lastToken(tree, info.sentinel) + 1 else info.main_token;
    const end = tree.firstToken(info.child_type);
    while (i < end) : (i += 1) {
        switch (token_tags[i]) {
            .keyword_allowzero => result.allowzero_token = i,
            .keyword_const => result.const_token = i,
            .keyword_volatile => result.volatile_token = i,
            .keyword_align => {
                std.debug.assert(info.align_node != 0);
                if (info.bit_range_end != 0) {
                    std.debug.assert(info.bit_range_start != 0);
                    i = lastToken(tree, info.bit_range_end) + 1;
                } else {
                    i = lastToken(tree, info.align_node) + 1;
                }
            },
            else => {},
        }
    }
    return result;
}

pub fn ptrTypeSimple(tree: Ast, node: Node.Index) full.PtrType {
    std.debug.assert(tree.nodes.items(.tag)[node] == .ptr_type);
    const data = tree.nodes.items(.data)[node];
    const extra = tree.extraData(data.lhs, Node.PtrType);
    return fullPtrType(tree, .{
        .main_token = tree.nodes.items(.main_token)[node],
        .align_node = extra.align_node,
        .addrspace_node = extra.addrspace_node,
        .sentinel = extra.sentinel,
        .bit_range_start = 0,
        .bit_range_end = 0,
        .child_type = data.rhs,
    });
}

pub fn ptrTypeSentinel(tree: Ast, node: Node.Index) full.PtrType {
    std.debug.assert(tree.nodes.items(.tag)[node] == .ptr_type_sentinel);
    const data = tree.nodes.items(.data)[node];
    return fullPtrType(tree, .{
        .main_token = tree.nodes.items(.main_token)[node],
        .align_node = 0,
        .addrspace_node = 0,
        .sentinel = data.lhs,
        .bit_range_start = 0,
        .bit_range_end = 0,
        .child_type = data.rhs,
    });
}

pub fn ptrTypeAligned(tree: Ast, node: Node.Index) full.PtrType {
    std.debug.assert(tree.nodes.items(.tag)[node] == .ptr_type_aligned);
    const data = tree.nodes.items(.data)[node];
    return fullPtrType(tree, .{
        .main_token = tree.nodes.items(.main_token)[node],
        .align_node = data.lhs,
        .addrspace_node = 0,
        .sentinel = 0,
        .bit_range_start = 0,
        .bit_range_end = 0,
        .child_type = data.rhs,
    });
}

pub fn ptrTypeBitRange(tree: Ast, node: Node.Index) full.PtrType {
    std.debug.assert(tree.nodes.items(.tag)[node] == .ptr_type_bit_range);
    const data = tree.nodes.items(.data)[node];
    const extra = tree.extraData(data.lhs, Node.PtrTypeBitRange);
    return fullPtrType(tree, .{
        .main_token = tree.nodes.items(.main_token)[node],
        .align_node = extra.align_node,
        .addrspace_node = extra.addrspace_node,
        .sentinel = extra.sentinel,
        .bit_range_start = extra.bit_range_start,
        .bit_range_end = extra.bit_range_end,
        .child_type = data.rhs,
    });
}

fn fullIf(tree: Ast, info: full.If.Components) full.If {
    const token_tags = tree.tokens.items(.tag);
    var result: full.If = .{
        .ast = info,
        .payload_token = null,
        .error_token = null,
        .else_token = undefined,
    };
    // if (cond_expr) |x|
    //              ^ ^
    const payload_pipe = lastToken(tree, info.cond_expr) + 2;
    if (token_tags[payload_pipe] == .pipe) {
        result.payload_token = payload_pipe + 1;
    }
    if (info.else_expr != 0) {
        // then_expr else |x|
        //           ^    ^
        result.else_token = lastToken(tree, info.then_expr) + 1;
        if (token_tags[result.else_token + 1] == .pipe) {
            result.error_token = result.else_token + 2;
        }
    }
    return result;
}

pub fn ifFull(tree: Ast, node: Node.Index) full.If {
    const data = tree.nodes.items(.data)[node];
    if (tree.nodes.items(.tag)[node] == .@"if") {
        const extra = tree.extraData(data.rhs, Node.If);
        return fullIf(tree, .{
            .cond_expr = data.lhs,
            .then_expr = extra.then_expr,
            .else_expr = extra.else_expr,
            .if_token = tree.nodes.items(.main_token)[node],
        });
    } else {
        std.debug.assert(tree.nodes.items(.tag)[node] == .if_simple);
        return fullIf(tree, .{
            .cond_expr = data.lhs,
            .then_expr = data.rhs,
            .else_expr = 0,
            .if_token = tree.nodes.items(.main_token)[node],
        });
    }
}

fn fullWhile(tree: Ast, info: full.While.Components) full.While {
    const token_tags = tree.tokens.items(.tag);
    var result: full.While = .{
        .ast = info,
        .inline_token = null,
        .label_token = null,
        .payload_token = null,
        .else_token = undefined,
        .error_token = null,
    };
    var tok_i = info.while_token - 1;
    if (token_tags[tok_i] == .keyword_inline) {
        result.inline_token = tok_i;
        tok_i -= 1;
    }
    if (token_tags[tok_i] == .colon and
        token_tags[tok_i - 1] == .identifier)
    {
        result.label_token = tok_i - 1;
    }
    const last_cond_token = lastToken(tree, info.cond_expr);
    if (token_tags[last_cond_token + 2] == .pipe) {
        result.payload_token = last_cond_token + 3;
    }
    if (info.else_expr != 0) {
        // then_expr else |x|
        //           ^    ^
        result.else_token = lastToken(tree, info.then_expr) + 1;
        if (token_tags[result.else_token + 1] == .pipe) {
            result.error_token = result.else_token + 2;
        }
    }
    return result;
}

pub fn whileSimple(tree: Ast, node: Node.Index) full.While {
    const data = tree.nodes.items(.data)[node];
    return fullWhile(tree, .{
        .while_token = tree.nodes.items(.main_token)[node],
        .cond_expr = data.lhs,
        .cont_expr = 0,
        .then_expr = data.rhs,
        .else_expr = 0,
    });
}

pub fn whileCont(tree: Ast, node: Node.Index) full.While {
    const data = tree.nodes.items(.data)[node];
    const extra = tree.extraData(data.rhs, Node.WhileCont);
    return fullWhile(tree, .{
        .while_token = tree.nodes.items(.main_token)[node],
        .cond_expr = data.lhs,
        .cont_expr = extra.cont_expr,
        .then_expr = extra.then_expr,
        .else_expr = 0,
    });
}

pub fn whileFull(tree: Ast, node: Node.Index) full.While {
    const data = tree.nodes.items(.data)[node];
    const extra = tree.extraData(data.rhs, Node.While);
    return fullWhile(tree, .{
        .while_token = tree.nodes.items(.main_token)[node],
        .cond_expr = data.lhs,
        .cont_expr = extra.cont_expr,
        .then_expr = extra.then_expr,
        .else_expr = extra.else_expr,
    });
}

pub fn forSimple(tree: Ast, node: Node.Index) full.While {
    const data = tree.nodes.items(.data)[node];
    return fullWhile(tree, .{
        .while_token = tree.nodes.items(.main_token)[node],
        .cond_expr = data.lhs,
        .cont_expr = 0,
        .then_expr = data.rhs,
        .else_expr = 0,
    });
}

pub fn forFull(tree: Ast, node: Node.Index) full.While {
    const data = tree.nodes.items(.data)[node];
    const extra = tree.extraData(data.rhs, Node.If);
    return fullWhile(tree, .{
        .while_token = tree.nodes.items(.main_token)[node],
        .cond_expr = data.lhs,
        .cont_expr = 0,
        .then_expr = extra.then_expr,
        .else_expr = extra.else_expr,
    });
}

pub fn lastToken(tree: Ast, node: Ast.Node.Index) Ast.TokenIndex {
    const TokenIndex = Ast.TokenIndex;
    const tags = tree.nodes.items(.tag);
    const datas = tree.nodes.items(.data);
    const main_tokens = tree.nodes.items(.main_token);
    const token_starts = tree.tokens.items(.start);
    const token_tags = tree.tokens.items(.tag);
    var n = node;
    var end_offset: TokenIndex = 0;
    while (true) switch (tags[n]) {
        .root => return @intCast(TokenIndex, tree.tokens.len - 1),
        .@"usingnamespace" => {
            // lhs is the expression
            if (datas[n].lhs == 0) {
                return main_tokens[n] + end_offset;
            } else {
                n = datas[n].lhs;
            }
        },
        .test_decl => {
            // rhs is the block
            // lhs is the name
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                n = datas[n].lhs;
            } else {
                return main_tokens[n] + end_offset;
            }
        },
        .global_var_decl => {
            // rhs is init node
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else {
                const extra = tree.extraData(datas[n].lhs, Node.GlobalVarDecl);
                if (extra.section_node != 0) {
                    end_offset += 1; // for the rparen
                    n = extra.section_node;
                } else if (extra.align_node != 0) {
                    end_offset += 1; // for the rparen
                    n = extra.align_node;
                } else if (extra.type_node != 0) {
                    n = extra.type_node;
                } else {
                    end_offset += 1; // from mut token to name
                    return main_tokens[n] + end_offset;
                }
            }
        },
        .local_var_decl => {
            // rhs is init node
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else {
                const extra = tree.extraData(datas[n].lhs, Node.LocalVarDecl);
                if (extra.align_node != 0) {
                    end_offset += 1; // for the rparen
                    n = extra.align_node;
                } else if (extra.type_node != 0) {
                    n = extra.type_node;
                } else {
                    end_offset += 1; // from mut token to name
                    return main_tokens[n] + end_offset;
                }
            }
        },
        .simple_var_decl => {
            // rhs is init node
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                n = datas[n].lhs;
            } else {
                end_offset += 1; // from mut token to name
                return main_tokens[n] + end_offset;
            }
        },
        .aligned_var_decl => {
            // rhs is init node, lhs is align node
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                end_offset += 1; // for the rparen
                n = datas[n].lhs;
            } else {
                end_offset += 1; // from mut token to name
                return main_tokens[n] + end_offset;
            }
        },
        .@"errdefer" => {
            // lhs is the token payload, rhs is the expression
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                // right pipe
                end_offset += 1;
                n = datas[n].lhs;
            } else {
                return main_tokens[n] + end_offset;
            }
        },
        .@"defer" => {
            // rhs is the defered expr
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else {
                return main_tokens[n] + end_offset;
            }
        },

        .bool_not,
        .negation,
        .bit_not,
        .negation_wrap,
        .address_of,
        .@"try",
        .@"await",
        .optional_type,
        .@"resume",
        .@"nosuspend",
        .@"comptime",
        => n = datas[n].lhs,

        .@"catch",
        .equal_equal,
        .bang_equal,
        .less_than,
        .greater_than,
        .less_or_equal,
        .greater_or_equal,
        .assign_mul,
        .assign_div,
        .assign_mod,
        .assign_add,
        .assign_sub,
        .assign_shl,
        .assign_shr,
        .assign_bit_and,
        .assign_bit_xor,
        .assign_bit_or,
        .assign_mul_wrap,
        .assign_add_wrap,
        .assign_sub_wrap,
        .assign_mul_sat,
        .assign_add_sat,
        .assign_sub_sat,
        .assign_shl_sat,
        .assign,
        .merge_error_sets,
        .mul,
        .div,
        .mod,
        .array_mult,
        .mul_wrap,
        .mul_sat,
        .add,
        .sub,
        .array_cat,
        .add_wrap,
        .sub_wrap,
        .add_sat,
        .sub_sat,
        .shl,
        .shl_sat,
        .shr,
        .bit_and,
        .bit_xor,
        .bit_or,
        .@"orelse",
        .bool_and,
        .bool_or,
        .anyframe_type,
        .error_union,
        .if_simple,
        .while_simple,
        .for_simple,
        .ptr_type_aligned,
        .ptr_type_sentinel,
        .ptr_type,
        .ptr_type_bit_range,
        .array_type,
        .switch_case_one,
        .switch_case,
        .switch_case_inline_one,
        .switch_case_inline,
        .switch_range,
        => n = datas[n].rhs,

        .field_access,
        .unwrap_optional,
        .grouped_expression,
        .multiline_string_literal,
        .error_set_decl,
        .asm_simple,
        .asm_output,
        .asm_input,
        => return datas[n].rhs + end_offset,

        .error_value => {
            if (datas[n].rhs != 0) {
                return datas[n].rhs + end_offset;
            } else if (datas[n].lhs != 0) {
                return datas[n].lhs + end_offset;
            } else {
                return main_tokens[n] + end_offset;
            }
        },

        .anyframe_literal,
        .char_literal,
        .number_literal,
        // .float_literal,
        .unreachable_literal,
        .identifier,
        .deref,
        .enum_literal,
        .string_literal,
        => return main_tokens[n] + end_offset,

        .@"return" => if (datas[n].lhs != 0) {
            n = datas[n].lhs;
        } else {
            return main_tokens[n] + end_offset;
        },

        .call, .async_call => {
            end_offset += 1; // for the rparen
            const params = tree.extraData(datas[n].rhs, Node.SubRange);
            if (params.end - params.start == 0) {
                return main_tokens[n] + end_offset;
            }
            n = tree.extra_data[params.end - 1]; // last parameter
        },
        .tagged_union_enum_tag => {
            const members = tree.extraData(datas[n].rhs, Node.SubRange);
            if (members.end - members.start == 0) {
                end_offset += 4; // for the rparen + rparen + lbrace + rbrace
                n = datas[n].lhs;
            } else {
                end_offset += 1; // for the rbrace
                n = tree.extra_data[members.end - 1]; // last parameter
            }
        },
        .call_comma,
        .async_call_comma,
        .tagged_union_enum_tag_trailing,
        => {
            end_offset += 2; // for the comma/semicolon + rparen/rbrace
            const params = tree.extraData(datas[n].rhs, Node.SubRange);
            std.debug.assert(params.end > params.start);
            n = tree.extra_data[params.end - 1]; // last parameter
        },
        .@"switch" => {
            const cases = tree.extraData(datas[n].rhs, Node.SubRange);
            if (cases.end - cases.start == 0) {
                end_offset += 3; // rparen, lbrace, rbrace
                n = datas[n].lhs; // condition expression
            } else {
                end_offset += 1; // for the rbrace
                n = tree.extra_data[cases.end - 1]; // last case
            }
        },
        .container_decl_arg => {
            const members = tree.extraData(datas[n].rhs, Node.SubRange);
            if (members.end - members.start == 0) {
                end_offset += 3; // for the rparen + lbrace + rbrace
                n = datas[n].lhs;
            } else {
                end_offset += 1; // for the rbrace
                n = tree.extra_data[members.end - 1]; // last parameter
            }
        },
        .@"asm" => {
            const extra = tree.extraData(datas[n].rhs, Node.Asm);
            return extra.rparen + end_offset;
        },
        .array_init,
        .struct_init,
        => {
            const elements = tree.extraData(datas[n].rhs, Node.SubRange);
            std.debug.assert(elements.end - elements.start > 0);
            end_offset += 1; // for the rbrace
            n = tree.extra_data[elements.end - 1]; // last element
        },
        .array_init_comma,
        .struct_init_comma,
        .container_decl_arg_trailing,
        .switch_comma,
        => {
            const members = tree.extraData(datas[n].rhs, Node.SubRange);
            std.debug.assert(members.end - members.start > 0);
            end_offset += 2; // for the comma + rbrace
            n = tree.extra_data[members.end - 1]; // last parameter
        },
        .array_init_dot,
        .struct_init_dot,
        .block,
        .container_decl,
        .tagged_union,
        .builtin_call,
        => {
            std.debug.assert(datas[n].rhs - datas[n].lhs > 0);
            end_offset += 1; // for the rbrace
            n = tree.extra_data[datas[n].rhs - 1]; // last statement
        },
        .array_init_dot_comma,
        .struct_init_dot_comma,
        .block_semicolon,
        .container_decl_trailing,
        .tagged_union_trailing,
        .builtin_call_comma,
        => {
            std.debug.assert(datas[n].rhs - datas[n].lhs > 0);
            end_offset += 2; // for the comma/semicolon + rbrace/rparen
            n = tree.extra_data[datas[n].rhs - 1]; // last member
        },
        .call_one,
        .async_call_one,
        .array_access,
        => {
            end_offset += 1; // for the rparen/rbracket
            if (datas[n].rhs == 0) {
                return main_tokens[n] + end_offset;
            }
            n = datas[n].rhs;
        },
        .array_init_dot_two,
        .block_two,
        .builtin_call_two,
        .struct_init_dot_two,
        .container_decl_two,
        .tagged_union_two,
        => {
            if (datas[n].rhs != 0) {
                end_offset += 1; // for the rparen/rbrace
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                end_offset += 1; // for the rparen/rbrace
                n = datas[n].lhs;
            } else {
                switch (tags[n]) {
                    .array_init_dot_two,
                    .block_two,
                    .struct_init_dot_two,
                    => end_offset += 1, // rbrace
                    .builtin_call_two => end_offset += 2, // lparen/lbrace + rparen/rbrace
                    .container_decl_two => {
                        var i: u32 = 2; // lbrace + rbrace
                        while (token_tags[main_tokens[n] + i] == .container_doc_comment) i += 1;
                        end_offset += i;
                    },
                    .tagged_union_two => {
                        var i: u32 = 5; // (enum) {}
                        while (token_tags[main_tokens[n] + i] == .container_doc_comment) i += 1;
                        end_offset += i;
                    },
                    else => unreachable,
                }
                return main_tokens[n] + end_offset;
            }
        },
        .array_init_dot_two_comma,
        .builtin_call_two_comma,
        .block_two_semicolon,
        .struct_init_dot_two_comma,
        .container_decl_two_trailing,
        .tagged_union_two_trailing,
        => {
            end_offset += 2; // for the comma/semicolon + rbrace/rparen
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                n = datas[n].lhs;
            } else {
                return main_tokens[n] + end_offset; // returns { }
            }
        },
        .container_field_init => {
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                n = datas[n].lhs;
            } else {
                return main_tokens[n] + end_offset;
            }
        },
        .container_field_align => {
            if (datas[n].rhs != 0) {
                end_offset += 1; // for the rparen
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                n = datas[n].lhs;
            } else {
                return main_tokens[n] + end_offset;
            }
        },
        .container_field => {
            const extra = tree.extraData(datas[n].rhs, Node.ContainerField);
            if (extra.value_expr != 0) {
                n = extra.value_expr;
            } else if (extra.align_expr != 0) {
                end_offset += 1; // for the rparen
                n = extra.align_expr;
            } else if (datas[n].lhs != 0) {
                n = datas[n].lhs;
            } else {
                return main_tokens[n] + end_offset;
            }
        },

        .array_init_one,
        .struct_init_one,
        => {
            end_offset += 1; // rbrace
            if (datas[n].rhs == 0) {
                return main_tokens[n] + end_offset;
            } else {
                n = datas[n].rhs;
            }
        },
        .slice_open,
        .call_one_comma,
        .async_call_one_comma,
        .array_init_one_comma,
        .struct_init_one_comma,
        => {
            end_offset += 2; // ellipsis2 + rbracket, or comma + rparen
            n = datas[n].rhs;
            std.debug.assert(n != 0);
        },
        .slice => {
            const extra = tree.extraData(datas[n].rhs, Node.Slice);
            std.debug.assert(extra.end != 0); // should have used slice_open
            end_offset += 1; // rbracket
            n = extra.end;
        },
        .slice_sentinel => {
            const extra = tree.extraData(datas[n].rhs, Node.SliceSentinel);
            if (extra.sentinel != 0) {
                end_offset += 1; // right bracket
                n = extra.sentinel;
            } else if (extra.end != 0) {
                end_offset += 2; // colon, right bracket
                n = extra.end;
            } else {
                // Assume both sentinel and end are completely devoid of tokens
                end_offset += 3; // ellipsis, colon, right bracket
                n = extra.start;
            }
        },

        .@"continue" => {
            if (datas[n].lhs != 0) {
                return datas[n].lhs + end_offset;
            } else {
                return main_tokens[n] + end_offset;
            }
        },
        .@"break" => {
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                return datas[n].lhs + end_offset;
            } else {
                return main_tokens[n] + end_offset;
            }
        },
        .fn_decl => {
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else {
                n = datas[n].lhs;
            }
        },
        .fn_proto_multi => {
            const extra = tree.extraData(datas[n].lhs, Node.SubRange);
            // rhs can be 0 when no return type is provided
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else {
                // Use the last argument and skip right paren
                n = tree.extra_data[extra.end - 1];
                end_offset += 1;
            }
        },
        .fn_proto_simple => {
            // rhs can be 0 when no return type is provided
            // lhs can be 0 when no parameter is provided
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                n = datas[n].lhs;
                // Skip right paren
                end_offset += 1;
            } else {
                // Skip left and right paren
                return main_tokens[n] + end_offset + 2;
            }
        },
        .fn_proto_one => {
            const extra = tree.extraData(datas[n].lhs, Node.FnProtoOne);
            // addrspace, linksection, callconv, align can appear in any order, so we
            // find the last one here.
            // rhs can be zero if no return type is provided
            var max_node: Node.Index = 0;
            var max_start: u32 = 0;
            if (datas[n].rhs != 0) {
                max_node = datas[n].rhs;
                max_start = token_starts[main_tokens[max_node]];
            }

            var max_offset: TokenIndex = 0;
            if (extra.align_expr != 0) {
                const start = token_starts[main_tokens[extra.align_expr]];
                if (start > max_start) {
                    max_node = extra.align_expr;
                    max_start = start;
                    max_offset = 1; // for the rparen
                }
            }
            if (extra.addrspace_expr != 0) {
                const start = token_starts[main_tokens[extra.addrspace_expr]];
                if (start > max_start) {
                    max_node = extra.addrspace_expr;
                    max_start = start;
                    max_offset = 1; // for the rparen
                }
            }
            if (extra.section_expr != 0) {
                const start = token_starts[main_tokens[extra.section_expr]];
                if (start > max_start) {
                    max_node = extra.section_expr;
                    max_start = start;
                    max_offset = 1; // for the rparen
                }
            }
            if (extra.callconv_expr != 0) {
                const start = token_starts[main_tokens[extra.callconv_expr]];
                if (start > max_start) {
                    max_node = extra.callconv_expr;
                    max_start = start;
                    max_offset = 1; // for the rparen
                }
            }

            if (max_node == 0) {
                std.debug.assert(max_offset == 0);
                // No linksection, callconv, align, return type
                if (extra.param != 0) {
                    n = extra.param;
                    end_offset += 1;
                } else {
                    // Skip left and right parens
                    return main_tokens[n] + end_offset + 2;
                }
            } else {
                n = max_node;
                end_offset += max_offset;
            }
        },
        .fn_proto => {
            const extra = tree.extraData(datas[n].lhs, Node.FnProto);
            // addrspace, linksection, callconv, align can appear in any order, so we
            // find the last one here.
            // rhs can be zero if no return type is provided
            var max_node: Node.Index = 0;
            var max_start: u32 = 0;
            if (datas[n].rhs != 0) {
                max_node = datas[n].rhs;
                max_start = token_starts[main_tokens[max_node]];
            }

            var max_offset: TokenIndex = 0;
            if (extra.align_expr != 0) {
                const start = token_starts[main_tokens[extra.align_expr]];
                if (start > max_start) {
                    max_node = extra.align_expr;
                    max_start = start;
                    max_offset = 1; // for the rparen
                }
            }
            if (extra.addrspace_expr != 0) {
                const start = token_starts[main_tokens[extra.addrspace_expr]];
                if (start > max_start) {
                    max_node = extra.addrspace_expr;
                    max_start = start;
                    max_offset = 1; // for the rparen
                }
            }
            if (extra.section_expr != 0) {
                const start = token_starts[main_tokens[extra.section_expr]];
                if (start > max_start) {
                    max_node = extra.section_expr;
                    max_start = start;
                    max_offset = 1; // for the rparen
                }
            }
            if (extra.callconv_expr != 0) {
                const start = token_starts[main_tokens[extra.callconv_expr]];
                if (start > max_start) {
                    max_node = extra.callconv_expr;
                    max_start = start;
                    max_offset = 1; // for the rparen
                }
            }
            if (max_node == 0) {
                std.debug.assert(max_offset == 0);
                // No linksection, callconv, align, return type
                // Use the last parameter and skip one extra token for the right paren
                n = extra.params_end;
                end_offset += 1;
            } else {
                n = max_node;
                end_offset += max_offset;
            }
        },
        .while_cont => {
            const extra = tree.extraData(datas[n].rhs, Node.WhileCont);
            std.debug.assert(extra.then_expr != 0);
            n = extra.then_expr;
        },
        .@"while" => {
            const extra = tree.extraData(datas[n].rhs, Node.While);
            std.debug.assert(extra.else_expr != 0);
            n = extra.else_expr;
        },
        .@"if", .@"for" => {
            const extra = tree.extraData(datas[n].rhs, Node.If);
            std.debug.assert(extra.else_expr != 0);
            n = extra.else_expr;
        },
        .@"suspend" => {
            if (datas[n].lhs != 0) {
                n = datas[n].lhs;
            } else {
                return main_tokens[n] + end_offset;
            }
        },
        .array_type_sentinel => {
            const extra = tree.extraData(datas[n].rhs, Node.ArrayTypeSentinel);
            n = extra.elem_type;
        },
    };
}

pub fn containerField(tree: Ast, node: Ast.Node.Index) ?Ast.full.ContainerField {
    return switch (tree.nodes.items(.tag)[node]) {
        .container_field => tree.containerField(node),
        .container_field_init => tree.containerFieldInit(node),
        .container_field_align => tree.containerFieldAlign(node),
        else => null,
    };
}

pub fn ptrType(tree: Ast, node: Ast.Node.Index) ?Ast.full.PtrType {
    return switch (tree.nodes.items(.tag)[node]) {
        .ptr_type => ptrTypeSimple(tree, node),
        .ptr_type_aligned => ptrTypeAligned(tree, node),
        .ptr_type_bit_range => ptrTypeBitRange(tree, node),
        .ptr_type_sentinel => ptrTypeSentinel(tree, node),
        else => null,
    };
}

pub fn whileAst(tree: Ast, node: Ast.Node.Index) ?Ast.full.While {
    return switch (tree.nodes.items(.tag)[node]) {
        .@"while" => whileFull(tree, node),
        .while_simple => whileSimple(tree, node),
        .while_cont => whileCont(tree, node),
        .@"for" => forFull(tree, node),
        .for_simple => forSimple(tree, node),
        else => null,
    };
}

pub fn isContainer(tree: Ast, node: Ast.Node.Index) bool {
    return switch (tree.nodes.items(.tag)[node]) {
        .container_decl,
        .container_decl_trailing,
        .container_decl_arg,
        .container_decl_arg_trailing,
        .container_decl_two,
        .container_decl_two_trailing,
        .tagged_union,
        .tagged_union_trailing,
        .tagged_union_two,
        .tagged_union_two_trailing,
        .tagged_union_enum_tag,
        .tagged_union_enum_tag_trailing,
        .root,
        .error_set_decl,
        => true,
        else => false,
    };
}

pub fn containerDecl(tree: Ast, node_idx: Ast.Node.Index, buffer: *[2]Ast.Node.Index) ?full.ContainerDecl {
    return switch (tree.nodes.items(.tag)[node_idx]) {
        .container_decl, .container_decl_trailing => tree.containerDecl(node_idx),
        .container_decl_arg, .container_decl_arg_trailing => tree.containerDeclArg(node_idx),
        .container_decl_two, .container_decl_two_trailing => tree.containerDeclTwo(buffer, node_idx),
        .tagged_union, .tagged_union_trailing => tree.taggedUnion(node_idx),
        .tagged_union_enum_tag, .tagged_union_enum_tag_trailing => tree.taggedUnionEnumTag(node_idx),
        .tagged_union_two, .tagged_union_two_trailing => tree.taggedUnionTwo(buffer, node_idx),
        else => null,
    };
}

/// Returns the member indices of a given declaration container.
/// Asserts given `tag` is a container node
pub fn declMembers(tree: Ast, node_idx: Ast.Node.Index, buffer: *[2]Ast.Node.Index) []const Ast.Node.Index {
    std.debug.assert(isContainer(tree, node_idx));
    return switch (tree.nodes.items(.tag)[node_idx]) {
        .container_decl, .container_decl_trailing => tree.containerDecl(node_idx).ast.members,
        .container_decl_arg, .container_decl_arg_trailing => tree.containerDeclArg(node_idx).ast.members,
        .container_decl_two, .container_decl_two_trailing => tree.containerDeclTwo(buffer, node_idx).ast.members,
        .tagged_union, .tagged_union_trailing => tree.taggedUnion(node_idx).ast.members,
        .tagged_union_enum_tag, .tagged_union_enum_tag_trailing => tree.taggedUnionEnumTag(node_idx).ast.members,
        .tagged_union_two, .tagged_union_two_trailing => tree.taggedUnionTwo(buffer, node_idx).ast.members,
        .root => tree.rootDecls(),
        .error_set_decl => &[_]Ast.Node.Index{},
        else => unreachable,
    };
}

/// Returns an `ast.full.VarDecl` for a given node index.
/// Returns null if the tag doesn't match
pub fn varDecl(tree: Ast, node_idx: Ast.Node.Index) ?Ast.full.VarDecl {
    return switch (tree.nodes.items(.tag)[node_idx]) {
        .global_var_decl => tree.globalVarDecl(node_idx),
        .local_var_decl => tree.localVarDecl(node_idx),
        .aligned_var_decl => tree.alignedVarDecl(node_idx),
        .simple_var_decl => tree.simpleVarDecl(node_idx),
        else => null,
    };
}

pub fn isPtrType(tree: Ast, node: Ast.Node.Index) bool {
    return switch (tree.nodes.items(.tag)[node]) {
        .ptr_type,
        .ptr_type_aligned,
        .ptr_type_bit_range,
        .ptr_type_sentinel,
        => true,
        else => false,
    };
}

pub fn isBuiltinCall(tree: Ast, node: Ast.Node.Index) bool {
    return switch (tree.nodes.items(.tag)[node]) {
        .builtin_call,
        .builtin_call_comma,
        .builtin_call_two,
        .builtin_call_two_comma,
        => true,
        else => false,
    };
}

pub fn isCall(tree: Ast, node: Ast.Node.Index) bool {
    return switch (tree.nodes.items(.tag)[node]) {
        .call,
        .call_comma,
        .call_one,
        .call_one_comma,
        .async_call,
        .async_call_comma,
        .async_call_one,
        .async_call_one_comma,
        => true,
        else => false,
    };
}

pub fn isBlock(tree: Ast, node: Ast.Node.Index) bool {
    return switch (tree.nodes.items(.tag)[node]) {
        .block_two,
        .block_two_semicolon,
        .block,
        .block_semicolon,
        => true,
        else => false,
    };
}

pub fn fnProtoHasBody(tree: Ast, node: Ast.Node.Index) ?bool {
    return switch (tree.nodes.items(.tag)[node]) {
        .fn_proto,
        .fn_proto_multi,
        .fn_proto_one,
        .fn_proto_simple,
        => false,
        .fn_decl => true,
        else => null,
    };
}

pub fn fnProto(tree: Ast, node: Ast.Node.Index, buf: *[1]Ast.Node.Index) ?Ast.full.FnProto {
    return switch (tree.nodes.items(.tag)[node]) {
        .fn_proto => tree.fnProto(node),
        .fn_proto_multi => tree.fnProtoMulti(node),
        .fn_proto_one => tree.fnProtoOne(buf, node),
        .fn_proto_simple => tree.fnProtoSimple(buf, node),
        .fn_decl => fnProto(tree, tree.nodes.items(.data)[node].lhs, buf),
        else => null,
    };
}

pub fn callFull(tree: Ast, node: Ast.Node.Index, buf: *[1]Ast.Node.Index) ?Ast.full.Call {
    return switch (tree.nodes.items(.tag)[node]) {
        .call,
        .call_comma,
        .async_call,
        .async_call_comma,
        => tree.callFull(node),
        .call_one,
        .call_one_comma,
        .async_call_one,
        .async_call_one_comma,
        => tree.callOne(buf, node),
        else => null,
    };
}

/// returns a list of parameters
pub fn builtinCallParams(tree: Ast, node: Ast.Node.Index, buf: *[2]Ast.Node.Index) ?[]const Node.Index {
    const node_data = tree.nodes.items(.data);
    return switch (tree.nodes.items(.tag)[node]) {
        .builtin_call_two, .builtin_call_two_comma => {
            buf[0] = node_data[node].lhs;
            buf[1] = node_data[node].rhs;
            if (node_data[node].lhs == 0) {
                return buf[0..0];
            } else if (node_data[node].rhs == 0) {
                return buf[0..1];
            } else {
                return buf[0..2];
            }
        },
        .builtin_call,
        .builtin_call_comma,
        => tree.extra_data[node_data[node].lhs..node_data[node].rhs],
        else => return null,
    };
}

/// returns a list of statements
pub fn blockStatements(tree: Ast, node: Ast.Node.Index, buf: *[2]Ast.Node.Index) ?[]const Node.Index {
    const node_data = tree.nodes.items(.data);
    return switch (tree.nodes.items(.tag)[node]) {
        .block_two, .block_two_semicolon => {
            buf[0] = node_data[node].lhs;
            buf[1] = node_data[node].rhs;
            if (node_data[node].lhs == 0) {
                return buf[0..0];
            } else if (node_data[node].rhs == 0) {
                return buf[0..1];
            } else {
                return buf[0..2];
            }
        },
        .block,
        .block_semicolon,
        => tree.extra_data[node_data[node].lhs..node_data[node].rhs],
        else => return null,
    };
}

/// Iterates over FnProto Params w/ added bounds check to support incomplete ast nodes
pub fn nextFnParam(it: *Ast.full.FnProto.Iterator) ?Ast.full.FnProto.Param {
    const token_tags = it.tree.tokens.items(.tag);
    while (true) {
        var first_doc_comment: ?Ast.TokenIndex = null;
        var comptime_noalias: ?Ast.TokenIndex = null;
        var name_token: ?Ast.TokenIndex = null;
        if (!it.tok_flag) {
            if (it.param_i >= it.fn_proto.ast.params.len) {
                return null;
            }
            const param_type = it.fn_proto.ast.params[it.param_i];
            var tok_i = it.tree.firstToken(param_type) - 1;
            while (true) : (tok_i -= 1) switch (token_tags[tok_i]) {
                .colon => continue,
                .identifier => name_token = tok_i,
                .doc_comment => first_doc_comment = tok_i,
                .keyword_comptime, .keyword_noalias => comptime_noalias = tok_i,
                else => break,
            };
            it.param_i += 1;
            it.tok_i = it.tree.lastToken(param_type) + 1;

            // #boundsCheck
            // https://github.com/zigtools/zls/issues/567
            if (it.tree.lastToken(param_type) >= it.tree.tokens.len - 1)
                return Ast.full.FnProto.Param{
                    .first_doc_comment = first_doc_comment,
                    .comptime_noalias = comptime_noalias,
                    .name_token = name_token,
                    .anytype_ellipsis3 = null,
                    .type_expr = 0,
                };

            // Look for anytype and ... params afterwards.
            if (token_tags[it.tok_i] == .comma) {
                it.tok_i += 1;
            }
            it.tok_flag = true;
            return Ast.full.FnProto.Param{
                .first_doc_comment = first_doc_comment,
                .comptime_noalias = comptime_noalias,
                .name_token = name_token,
                .anytype_ellipsis3 = null,
                .type_expr = param_type,
            };
        }
        if (token_tags[it.tok_i] == .comma) {
            it.tok_i += 1;
        }
        if (token_tags[it.tok_i] == .r_paren) {
            return null;
        }
        if (token_tags[it.tok_i] == .doc_comment) {
            first_doc_comment = it.tok_i;
            while (token_tags[it.tok_i] == .doc_comment) {
                it.tok_i += 1;
            }
        }
        switch (token_tags[it.tok_i]) {
            .ellipsis3 => {
                it.tok_flag = false; // Next iteration should return null.
                return Ast.full.FnProto.Param{
                    .first_doc_comment = first_doc_comment,
                    .comptime_noalias = null,
                    .name_token = null,
                    .anytype_ellipsis3 = it.tok_i,
                    .type_expr = 0,
                };
            },
            .keyword_noalias, .keyword_comptime => {
                comptime_noalias = it.tok_i;
                it.tok_i += 1;
            },
            else => {},
        }
        if (token_tags[it.tok_i] == .identifier and
            token_tags[it.tok_i + 1] == .colon)
        {
            name_token = it.tok_i;
            it.tok_i += 2;
        }
        if (token_tags[it.tok_i] == .keyword_anytype) {
            it.tok_i += 1;
            return Ast.full.FnProto.Param{
                .first_doc_comment = first_doc_comment,
                .comptime_noalias = comptime_noalias,
                .name_token = name_token,
                .anytype_ellipsis3 = it.tok_i - 1,
                .type_expr = 0,
            };
        }
        it.tok_flag = false;
    }
}

/// A modified version of tree.tokenSlice that returns an error.UnexpectedToken if the tokenizer encounters an unexpected token
// https://github.com/zigtools/zls/issues/381
pub fn tokenSlice(tree: Ast, token_index: Ast.TokenIndex) ![]const u8 {
    const token_starts = tree.tokens.items(.start);
    const token_tags = tree.tokens.items(.tag);
    const token_tag = token_tags[token_index];

    // Many tokens can be determined entirely by their tag.
    if (token_tag.lexeme()) |lexeme| {
        return lexeme;
    }

    // For some tokens, re-tokenization is needed to find the end.
    var tokenizer: std.zig.Tokenizer = .{
        .buffer = tree.source,
        .index = token_starts[token_index],
        .pending_invalid_token = null,
    };
    const token = tokenizer.next();
    if (token.tag != token_tag) return error.UnexpectedToken; // assert(token.tag == token_tag);
    return tree.source[token.loc.start..token.loc.end];
}

pub fn tokenLocation(tree: Ast, token_index: Ast.TokenIndex) std.zig.Token.Loc {
    const start = tree.tokens.items(.start)[token_index];
    const tag = tree.tokens.items(.tag)[token_index];

    // For some tokens, re-tokenization is needed to find the end.
    var tokenizer: std.zig.Tokenizer = .{
        .buffer = tree.source,
        .index = start,
        .pending_invalid_token = null,
    };

    const token = tokenizer.next();
    // HACK, should return error.UnextectedToken
    if (token.tag != tag) return .{ .start = 0, .end = 0 }; //std.debug.assert(token.tag == tag);
    return .{ .start = token.loc.start, .end = token.loc.end };
}

pub fn getDeclNameToken(tree: Ast, node: Ast.Node.Index) ?Ast.TokenIndex {
    const tags = tree.nodes.items(.tag);
    const main_token = tree.nodes.items(.main_token)[node];
    return switch (tags[node]) {
        // regular declaration names. + 1 to mut token because name comes after 'const'/'var'
        .local_var_decl => tree.localVarDecl(node).ast.mut_token + 1,
        .global_var_decl => tree.globalVarDecl(node).ast.mut_token + 1,
        .simple_var_decl => tree.simpleVarDecl(node).ast.mut_token + 1,
        .aligned_var_decl => tree.alignedVarDecl(node).ast.mut_token + 1,
        // function declaration names
        .fn_proto,
        .fn_proto_multi,
        .fn_proto_one,
        .fn_proto_simple,
        .fn_decl,
        => blk: {
            var params: [1]Ast.Node.Index = undefined;
            break :blk fnProto(tree, node, &params).?.name_token;
        },

        // containers
        .container_field => if (tree.containerField(node).ast.tuple_like) null else tree.containerField(node).ast.main_token,
        .container_field_init => if (tree.containerFieldInit(node).ast.tuple_like) null else tree.containerFieldInit(node).ast.main_token,
        .container_field_align => if (tree.containerFieldAlign(node).ast.tuple_like) null else tree.containerFieldAlign(node).ast.main_token,

        .identifier => main_token,
        .error_value => main_token + 2, // 'error'.<main_token +2>

        // lhs of main token is name token, so use `node` - 1
        .test_decl => if (tree.tokens.items(.tag)[main_token + 1] == .string_literal)
            return main_token + 1
        else
            null,
        else => null,
    };
}

pub fn getDeclName(tree: Ast, node: Ast.Node.Index) ?[]const u8 {
    const name = tree.tokenSlice(getDeclNameToken(tree, node) orelse return null);
    return switch (tree.nodes.items(.tag)[node]) {
        .test_decl => name[1 .. name.len - 1],
        else => name,
    };
}

/// Gets a declaration's doc comments. Caller owns returned memory.
pub fn getDocComments(allocator: std.mem.Allocator, tree: Ast, node: Ast.Node.Index) !?std.ArrayListUnmanaged([]const u8) {
    const base = tree.nodes.items(.main_token)[node];
    const base_kind = tree.nodes.items(.tag)[node];
    const tokens = tree.tokens.items(.tag);

    switch (base_kind) {
        // As far as I know, this does not actually happen yet, but it
        // may come in useful.
        .root => return try collectDocComments(allocator, tree, 0, true),
        .fn_proto,
        .fn_proto_one,
        .fn_proto_simple,
        .fn_proto_multi,
        .fn_decl,
        .local_var_decl,
        .global_var_decl,
        .aligned_var_decl,
        .simple_var_decl,
        .container_field_init,
        => {
            if (getDocCommentTokenIndex(tokens, base)) |doc_comment_index|
                return try collectDocComments(allocator, tree, doc_comment_index, false);
        },
        else => {},
    }
    return null;
}

/// Get the first doc comment of a declaration.
fn getDocCommentTokenIndex(tokens: []std.zig.Token.Tag, base_token: Ast.TokenIndex) ?Ast.TokenIndex {
    var idx = base_token;
    if (idx == 0) return null;
    idx -= 1;
    if (tokens[idx] == .keyword_threadlocal and idx > 0) idx -= 1;
    if (tokens[idx] == .string_literal and idx > 1 and tokens[idx - 1] == .keyword_extern) idx -= 1;
    if (tokens[idx] == .keyword_extern and idx > 0) idx -= 1;
    if (tokens[idx] == .keyword_export and idx > 0) idx -= 1;
    if (tokens[idx] == .keyword_inline and idx > 0) idx -= 1;
    if (tokens[idx] == .keyword_pub and idx > 0) idx -= 1;

    // Find first doc comment token
    if (!(tokens[idx] == .doc_comment))
        return null;
    return while (tokens[idx] == .doc_comment) {
        if (idx == 0) break 0;
        idx -= 1;
    } else idx + 1;
}

fn collectDocComments(allocator: std.mem.Allocator, tree: Ast, doc_comments: Ast.TokenIndex, container_doc: bool) !std.ArrayListUnmanaged([]const u8) {
    var lines = std.ArrayListUnmanaged([]const u8){};
    const tokens = tree.tokens.items(.tag);

    var curr_line_tok = doc_comments;
    while (true) : (curr_line_tok += 1) {
        const comm = tokens[curr_line_tok];
        if ((container_doc and comm == .container_doc_comment) or (!container_doc and comm == .doc_comment)) {
            try lines.append(allocator, std.mem.trim(u8, tree.tokenSlice(curr_line_tok)[3..], &std.ascii.whitespace));
        } else break;
    }

    return lines;
}

// http://tools.ietf.org/html/rfc3986#section-2.2
const reserved_chars = &[_]u8{
    '!', '#', '$', '%', '&', '\'',
    '(', ')', '*', '+', ',', ':',
    ';', '=', '?', '@', '[', ']',
};

const reserved_escapes = blk: {
    var escapes: [reserved_chars.len][3]u8 = [_][3]u8{[_]u8{undefined} ** 3} ** reserved_chars.len;

    for (reserved_chars) |c, i| {
        escapes[i][0] = '%';
        _ = std.fmt.bufPrint(escapes[i][1..], "{X}", .{c}) catch unreachable;
    }
    break :blk &escapes;
};

/// Returns a URI from a path, caller owns the memory allocated with `allocator`
pub fn fromPath(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    if (path.len == 0) return "";
    const prefix = if (builtin.os.tag == .windows) "file:///" else "file://";

    var buf = std.ArrayListUnmanaged(u8){};
    try buf.appendSlice(allocator, prefix);

    for (path) |char| {
        if (char == std.fs.path.sep) {
            try buf.append(allocator, '/');
        } else if (std.mem.indexOfScalar(u8, reserved_chars, char)) |reserved| {
            try buf.appendSlice(allocator, &reserved_escapes[reserved]);
        } else {
            try buf.append(allocator, char);
        }
    }

    // On windows, we need to lowercase the drive name.
    if (builtin.os.tag == .windows) {
        if (buf.items.len > prefix.len + 1 and
            std.ascii.isAlphabetic(buf.items[prefix.len]) and
            std.mem.startsWith(u8, buf.items[prefix.len + 1 ..], "%3A"))
        {
            buf.items[prefix.len] = std.ascii.toLower(buf.items[prefix.len]);
        }
    }

    return buf.toOwnedSlice(allocator);
}
