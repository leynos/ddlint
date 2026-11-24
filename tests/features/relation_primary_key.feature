Feature: Typed access to relation primary-key expressions

  Scenario: Spec-form key exposes a typed tuple expression
    Given the DDlog source "input relation Book(row: BookRow) primary key (row) (row.author, row.title)"
    When the first relation's primary key is inspected
    Then the primary-key binders are "row"
    And the primary-key expression text is "(row.author, row.title)"
    And the primary-key expression renders as "(tuple (field row author) (field row title))"

  Scenario: List-form key has binders but no expression
    Given the DDlog source "input relation R(a: u32, b: u32) primary key (a, b)"
    When the first relation's primary key is inspected
    Then the primary-key binders are "a, b"
    And the primary key has no expression

  Scenario: Trailing comment is not part of the key expression
    Given the DDlog source "input relation R(x: R) primary key (x) x.id // the id"
    When the first relation's primary key is inspected
    Then the primary-key expression text is "x.id"

  Scenario: Malformed key expression keeps binders and reports a located error
    Given the DDlog source "input relation R(x: R) primary key (x) x + * 1"
    When the first relation's primary key is inspected
    Then the primary-key binders are "x"
    And parsing the primary-key expression fails within its span
    And the parse has no parser-level errors

  Scenario: Statement terminator after a key does not hide the next declaration
    Given the DDlog source "input relation R(x: u32) primary key (x); output relation S(x: u32)"
    When the relations are listed
    Then the relation names are "R, S"

  Scenario: Key on an output relation is rejected and not exposed
    Given the DDlog source "output relation R(x: u32) primary key (x)"
    When the first relation's primary key is inspected
    Then the relation has no primary-key clause
    And the parse reports the diagnostic "D-REL-006"

  Scenario: Relation without a key has no primary-key clause
    Given the DDlog source "input relation R(x: u32)"
    When the first relation's primary key is inspected
    Then the relation has no primary-key clause
