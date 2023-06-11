<img src="https://raw.githubusercontent.com/babashka/babashka/master/logo/babashka.svg" widht="200px" height="200px">

# babashka.el
Emacs interface for babashka tasks.

## Usage

### Simply call a task
Library provides one entry point `babashka-tasks`. If called from a buffer visiting a file in a project with `bb.edn` in it, it will look up the directory tree recursively until `bb.edn` is found, parses it for tasks and offer a menu to select a task to run:

![](./videos/1-simple.gif)

### Call a tasks with command line args
If your task accepts command line arguments, just type them after the task name:

![](./videos/3-args.gif)

### Call tasks from any directory
If you want to specify a `bb.edn` to run tasks from, simply call `babashka-tasks` with an interactive argument: `C-u M-x babashka-tasks RET`:

![](./videos/2-interactive.gif)

## Installation
You can install the library using use-package and straight:

```elisp
(use-package babashka
  :straight (:type git :host github :repo "licht1stein/babashka.el"))
```

I will submit the library to MELPA once I test it a little.



