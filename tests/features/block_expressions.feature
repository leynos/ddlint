Feature: Block expressions and collection literals
  Braces delimit blocks whose value is their final item unless the block is
  empty or ends with a semicolon. Map literals use brackets and arrows.

  @BG-D3 @V-B5
  Scenario: Semicolons separate block statements in order
    Given the expression source "{ a; b; c }"
    When the expression is parsed
    Then the result is a block with statements
      | item |
      | a    |
      | b    |
    And the block value is "c"

  @BG-D3 @V-B1
  Scenario: A trailing semicolon makes the block value unit
    Given the expression source "{ a; }"
    When the expression is parsed
    Then the result is a block with statements
      | item |
      | a    |
    And the block has no value

  @BG-D4
  Scenario: Empty braces are an empty block
    Given the expression source "{}"
    When the expression is parsed
    Then the result is a block with no statements
    And the block has no value

  @BG-D11 @V-B2
  Scenario: An if expression inside a block does not swallow the separator
    Given the expression source "{ if c { a } else { b }; d }"
    When the expression is parsed
    Then the result is a block with statements
      | item                  |
      | if c { a } else { b } |
    And the block value is "d"

  @BG-D12 @V-M1 @V-B4
  Scenario Outline: Brackets build vectors and maps; braces build blocks
    Given the expression source "<source>"
    When the expression is parsed
    Then the result is a <form>

    Examples:
      | source           | form                |
      | [a, b]           | vector literal      |
      | [a -> 1, b -> 2] | map literal         |
      | { x }            | block               |
      | ( x )            | parenthesized group |
      | { x: T }         | block               |

  @V-B6 @V-M2
  Scenario Outline: Malformed braces and brackets are rejected
    Given the expression source "<source>"
    When the expression is parsed
    Then parsing fails with "<message>"

    Examples:
      | source       | message                                                   |
      | {a: 1, b: 2} | expected ';' or '}' after a block item                    |
      | [a, b -> c]  | cannot mix vector elements and map entries in one literal |
      | []           | empty vector literals are not DDlog syntax; use vec_empty() instead |
      | { a, b }     | expected ';' or '}' after a block item                    |
      | { a;; b }    | empty statement in block: remove the extra ';'            |

  @BG-D6
  Scenario: Braced if branches with an empty block parse
    Given the expression source "if ready {} else { go() }"
    When the expression is parsed
    Then the result is an if expression whose then branch is an empty block