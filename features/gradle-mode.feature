Feature: Execute Key Bindings
  In order to execute gradle commands
  As a user
  I want to enter key bindings to run gradle commands
  and maybe enter input after prompts appear

  Scenario: Run Build Command Without Daemon
    When Compile is mocked with "gradle build" and I press "C-c C-g b"
    # Nothing to check really, mock compile, as long as there are no 
    # mock errors, this passes

  Scenario: Run Test Command Without Daemon
    When Compile is mocked with "gradle test" and I press "C-c C-g t"
    # Nothing to check really, mock compile, as long as there are no 
    # mock errors, this passes

  Scenario: Run Build Command With Daemon
    When Compile is mocked with "gradle build --daemon" and I press "C-c C-g C-d b"
    # Nothing to check really, mock compile, as long as there are no 
    # mock errors, this passes

  Scenario: Run Test Command With Daemon
    When Compile is mocked with "gradle test --daemon" and I press "C-c C-g C-d t"
    # Nothing to check really, mock compile, as long as there are no 
    # mock errors, this passes

  Scenario: Run Single Test Without Daemon
    When Compile is mocked with "gradle test -Dtest.single=MySpec" and I press "C-c C-g s" and I type "MySpec" at the prompt

  Scenario: Run Single Test With Daemon
    When Compile is mocked with "gradle test -Dtest.single=MySpec --daemon" and I press "C-c C-g C-d s" and I type "MySpec" at the prompt

  Scenario: Execute User Prompted Task(s) Without Daemon
    When Compile is mocked with "gradle clean build" and I press "C-c C-g r" and I type "clean build" at the prompt

  Scenario: Execute User Prompted Task(s) With Daemon
    When Compile is mocked with "gradle clean build --daemon" and I press "C-c C-g d" and I type "clean build" at the prompt
