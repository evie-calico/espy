(integer) @number
(string) @string
(line_comment) @comment
"let" @keyword
"set" @keyword
"if" @keyword
"then" @keyword
"else" @keyword
"end" @keyword

"with" @function.macro

"(" @punctuation.bracket
")" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket

"." @punctuation.delimiter
":" @punctuation.delimiter
"," @punctuation.delimiter

"=" @operator
".*" @operator
"+" @operator
"-" @operator
"*" @operator
"/" @operator
"+" @operator
"-" @operator
"&" @operator
"^" @operator
"|" @operator
"==" @operator
"!=" @operator
">" @operator
">=" @operator
"<" @operator
"<=" @operator
"and" @operator
"or" @operator
"|>" @operator

((ident) @type
 (#match? @type "[A-Z][A-Za-z0-9_]+"))

(call_expression
  function: (binary_expression ) @function)
(field_expression
  field: (ident) @property)
