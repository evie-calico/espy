/**
 * @file espy grammar for tree-sitter
 * @author Evie Calico
 * @license MPL-2.0
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

function sepBy(sep, rule) {
  return optional(seq(rule, repeat(seq(sep, rule))));
}

module.exports = grammar({
  name: "espy",

  word: $ => $.ident,

  extras: $ => [
    /\s/,
    $.line_comment,
  ],

  rules: {
    source_file: $ => seq(
      // TODO: with
      repeat($.statement),
      optional($.expression),
    ),

    brace_block: $ => seq(
      '{',
      repeat($.statement),
      optional($.expression),
      '}',
    ),

    then_block: $ => seq(
      'then',
      repeat($.statement),
      optional($.expression),
    ),

    binding: $ => choice(
      $.ident,
      seq("(", sepBy(',', $.binding), ")"),
      seq("{", sepBy(',', seq($.ident, optional(seq(':', $.binding)))), "}"),
    ),

    statement: $ => choice(
      $.evaluation,
      $.set_expression,
      $.with_binding,
    ),

    evaluation: $ => seq(
      optional(seq('let', $.binding, '=')),
      $.expression,
      ';',
    ),

    set_expression: $ => seq('set', $.ident, '=', $.expression, ';'),
    with_binding: $ => seq('with', $.binding, ';'),

    expression: $ => choice(
      $.unary_expression,
      $.binary_expression,
    ),

    binary_expression: $ => choice(
      $.ident,
      $.integer,
      $.string,
      seq('(', $.expression, ')'),
      $.brace_block,

      $.if_expression,

      $.field_expression,
      $.dereference_expression,
      $.product_expression,
      $.sum_expression,
      $.bitwise_and_expression,
      $.bitwise_xor_expression,
      $.bitwise_or_expression,
      $.conditional_expression,
      $.logical_and_expression,
      $.logical_or_expression,

      $.name_expression,
      $.tuple_expression,
      $.pipe_expression,
      $.call_expression,
    ),

    if_expression: $ => seq(
      'if',
      $.expression,
      $.then_block,
      choice(
        seq('else', choice(
          $.if_expression,
          seq($.then_block, 'end'),
        )),
        'end',
      ),
    ),

    field_expression: $ => prec(13, seq(
      field("subject", $.binary_expression),
      '.',
      field("field", $.ident),
    )),
    dereference_expression: $ => prec(13, seq($.binary_expression, '.*')),
    unary_expression: $ => prec(12, choice(
      seq('+', $.expression),
      seq('-', $.expression),
    )),
    product_expression: $ => prec.left(11, choice(
      seq($.binary_expression, '*', $.expression),
      seq($.binary_expression, '/', $.expression),
    )),
    sum_expression: $ => prec.left(10, choice(
      seq($.binary_expression, '+', $.expression),
      seq($.binary_expression, '-', $.expression),
    )),
    bitwise_and_expression: $ => prec.left(9, seq($.binary_expression, '&', $.expression)),
    bitwise_xor_expression: $ => prec.left(8, seq($.binary_expression, '^', $.expression)),
    bitwise_or_expression: $ => prec.left(7, seq($.binary_expression, '|', $.expression)),
    conditional_expression: $ => prec.left(6, choice(
      seq($.binary_expression, '==', $.expression),
      seq($.binary_expression, '!=', $.expression),
      seq($.binary_expression, '>', $.expression),
      seq($.binary_expression, '>=', $.expression),
      seq($.binary_expression, '<', $.expression),
      seq($.binary_expression, '<=', $.expression),
    )),
    logical_and_expression: $ => prec.left(5, seq($.binary_expression, 'and', $.expression)),
    logical_or_expression: $ => prec.left(4, seq($.binary_expression, 'or', $.expression)),
    name_expression: $ => prec(3, seq($.ident, ':', $.expression)),
    tuple_expression: $ => prec.left(2, seq($.binary_expression, ',', $.expression)),
    call_expression: $ => prec.left(1, seq(
      field("function", $.binary_expression),
      field("argument", $.binary_expression),
    )),
    pipe_expression: $ => prec.left(1, seq($.binary_expression, '|>', $.expression)),

    line_comment: _ => /#.*/,
    
    ident: _ => /[a-zA-Z_][a-zA-Z_0-9]*/,
    integer: _ => /[0-9]+/,
    string: _ => /".*"/,
  }
});
