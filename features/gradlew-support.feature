Feature: Execute Key Bindings with gradlew

  Background:
    Given I set gradle-use-gradlew to t

  Scenario: Run gradlew Build Without Daemon
    When I press "C-c C-g b"
    Then Compilation directory is "gradle-mode-root-path"
    And Compilation command is "./gradlew build"

  Scenario: Run gradlew Test Without Daemon
    When I press "C-c C-g t"
    Then Compilation directory is "gradle-mode-root-path"
    And Compilation command is "./gradlew test"

  Scenario: Run gradlew Build Command With Daemon
    When I press "C-c C-g C-d b"
    Then Compilation directory is "gradle-mode-root-path"
    And Compilation command is "./gradlew build --daemon"

  Scenario: Run gradlew Test Command With Daemon
    When I press "C-c C-g C-d t"
    Then Compilation directory is "gradle-mode-root-path"
    And Compilation command is "./gradlew test --daemon"

  Scenario: Run gradlew Single Test Without Daemon
    Given I start an action chain
    And I press "C-c C-g s"
    And I type "MySpec"
    When I execute the action chain
    Then Compilation directory is "gradle-mode-root-path"
    And Compilation command is "./gradlew test -Dtest.single=MySpec"

  Scenario: Run gradlew Single Test With Daemon
    Given I start an action chain
    And I press "C-c C-g C-d s"
    And I type "MySpec"
    When I execute the action chain
    Then Compilation directory is "gradle-mode-root-path"
    And Compilation command is "./gradlew test -Dtest.single=MySpec --daemon"

  Scenario: Execute User Prompted Task(s) Without gradlew Daemon
    Given I start an action chain
    And I press "C-c C-g r"
    And I type "clean build"
    When I execute the action chain
    Then Compilation directory is "gradle-mode-root-path"
    And Compilation command is "./gradlew clean build"

  Scenario: Execute User Prompted Task(s) With gradlew Daemon
    Given I start an action chain
    And I press "C-c C-g d"
    And I type "clean build"
    When I execute the action chain
    Then Compilation directory is "gradle-mode-root-path"
    And Compilation command is "./gradlew clean build --daemon"
