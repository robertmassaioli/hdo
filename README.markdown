# htodo - ToDo List Organiser

htodo is a ToDo list organiser that is supposed to help you organise what you need to get done. It
supports (or will support) the following features:

 - Easy Command Line Interface (Not Implemented Yet)
 - Single Database Storage (Not Implemented Yet)
 - Taggable todo items. (Not Implemented Yet)
 - Parent-Child tree based structures for todo items. (Not Implemented Yet)
 - Records changes in the state of the todo items. (Not Implemented Yet)
 - Database upgrades should be easy for the user (Not Implemented Yet)

## Installation Instructions

htodo can be installed pretty easily. Simply check out the code and:

    cd /path/to/htodo
    cabal install

And that is all that there is to it. Any configuration or setup will be asked of you inside the
program itself. (Also just make sure that ~/.cabal/bin, or its windows equivalent, is in your path.)

## Usage Instructions

For now all of the instructions that are avaliable can be gathered via the command line --help
option. For the general overview of what this program does please just do the following:

    $ htodo --help=one

And it will print out all of your options.
