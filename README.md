# html-validator-cli [![CircleCI](https://circleci.com/gh/iij-ii/html-validator-cli.svg?style=svg)](https://circleci.com/gh/iij-ii/html-validator-cli)

Command Line Interface for [The W3C Markup Validation Service](https://validator.w3.org/)

```
$ cat /var/www/html/index.html
<!DOCTYPE html>
<html>
    <body>Hello, Validator!</body>
</html>

$ validatehtml /var/www/html/index.html
[ERROR] The character encoding was not declared. Proceeding using “windows-1252”.

[ERROR] Element “head” is missing a required instance of child element “title”.
From line 2, column 7; to line 3, column 10
ml> <html>     <body>Hello,
          ^^^^^^^^^^^

[WARNING] Consider adding a “lang” attribute to the “html” start tag to declare the language of this document.
From line 1, column 16; to line 2, column 6
TYPE html> <html>     <
          ^^^^^^^
```

## Installation

### Prerequisites

Make sure you have a fresh version of [Stack](https://docs.haskellstack.org/en/stable/README/) or [Cabal](https://www.haskell.org/cabal/) installed.

### Installation

```sh
stack update
stack install html-validator-cli
```
or
```sh
cabal update
cabal install html-validator-cli
```

## Usage

```
Usage: validatehtml [-s URL] [-x DIR] FILE | DIR ...
  -s URL  --validator-url=URL  validation service url (default: https://validator.w3.org/nu/)
  -x DIR  --exclude=DIR        exclude files in DIR
  -1      --oneline            print each message on one line
```
- `FILE | DIR ...`
    - Specify one or more HTML files to validate. When a directory is specified, all HTML files under that will be checked.
- `-s URL`, `--validator-url=URL`
    - Specify the alternative validation service that uses [The Nu Html Checker](https://validator.github.io/validator/) as its backend like follows:
        - https://checker.html5.org/
        - https://html5.validator.nu/
        - or your self-hosted The Nu Html Checker instance.
- `-x DIR`,  `--exclude=DIR`
    - Specify the folder name that you want to exclude from checking.
- `-1`, `--oneline`
    - Specify it if you want to print the validation result in compact format.

## License

Copyright (c) IIJ Innovation Institute Inc.

Licensed under The 3-Clause BSD License.
