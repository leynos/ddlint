Feature: rstest-bdd smoke binding

  Scenario: A single binder is exposed
    Given the source "input relation R(x: u32) primary key (x)"
    When its first primary key is inspected
    Then the binder count is 1
