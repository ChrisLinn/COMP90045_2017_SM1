# Todo

+ may need more works on lex
+ string type for writting
+ comments
+ draft precedence
+ error handling and line tracking
+ pprintf spec
    * [x] The pretty-printer should output the formatted procedures in the order they appeared in the input
    * [ ] Two consecutive procedure definitions should be separated by a single blank line
    * [x] The keywords proc and end should begin at the start of a line—no indentation
    * [x] Within each procedure, declarations and top-level statements should be indented by four spaces
    * [x] Each variable declaration should be on a separate line
    * [x] Each statement should start on a new line
    * [x] The declarations and the statements should be separated by a single blank line
    * [x] The statements inside a conditional or while loop should be indented four spaces beyond the conditional or while loop it is part of
    * [x] In a while statement, “while . . . do” should be printed on one line, irrespective of the size of the intervening expression. The same goes for “if . . . then”
    * [x] The terminating od should be indented exactly as the corresponding while. Similarly, the terminating fi (and a possible else) should be indented exactly as the corresponding if
    * [x] There should be no white space before a semicolon, and no white space before an opening square bracket
    * [ ] When printing expressions, you should not print any parentheses, except when omission of parentheses would change the meaning of the expression. For example, ((x) - (y - (4*z))) should be printed as x - (y - 4 * z)
    * [ ] White space should be preserved inside strings