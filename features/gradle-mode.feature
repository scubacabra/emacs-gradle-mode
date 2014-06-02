Feature: Execute Key Bindings
  In order to execute gradle commands
  As a user
  I want to enter key bindings to run gradle commands
  and maybe enter input after prompts appear
  then the command is run, and the compilation directory is appropriately set

  Scenario: Run Build Command Without Daemon
    When I press "C-c C-g b"
    Then Compilation directory is "gradle-mode-root-path"
    And Compilation command is "gradle build"

  Scenario: Run Test Command Without Daemon
    When I press "C-c C-g t"
    Then Compilation directory is "gradle-mode-root-path"
    And Compilation command is "gradle test"

  Scenario: Run Build Command With Daemon
    When I press "C-c C-g C-d b"
    Then Compilation directory is "gradle-mode-root-path"
    And Compilation command is "gradle build --daemon"

  Scenario: Run Test Command With Daemon
    When I press "C-c C-g C-d t"
    Then Compilation directory is "gradle-mode-root-path"
    And Compilation command is "gradle test --daemon"

  Scenario: Run Single Test Without Daemon
    Given I start an action chain
    And I press "C-c C-g s"
    And I type "MySpec"
    When I execute the action chain
    Then Compilation directory is "gradle-mode-root-path"
    And Compilation command is "gradle test -Dtest.single=MySpec"

  Scenario: Run Single Test With Daemon
    Given I start an action chain
    And I press "C-c C-g C-d s"
    And I type "MySpec"
    When I execute the action chain
    Then Compilation directory is "gradle-mode-root-path"
    And Compilation command is "gradle test -Dtest.single=MySpec --daemon"

  Scenario: Execute User Prompted Task(s) Without Daemon
    Given I start an action chain
    And I press "C-c C-g r"
    And I type "clean build"
    When I execute the action chain
    Then Compilation directory is "gradle-mode-root-path"
    And Compilation command is "gradle clean build"

  Scenario: Execute User Prompted Task(s) With Daemon
    Given I start an action chain
    And I press "C-c C-g d"
    And I type "clean build"
    When I execute the action chain
    Then Compilation directory is "gradle-mode-root-path"
    And Compilation command is "gradle clean build --daemon"
