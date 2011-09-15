# htodo - ToDo List Organiser

htodo is a command line based ToDo list organiser. It is supposed to help you organise what you need to get done. It
supports (or will support) the following features:

 - Easy Command Line Interface [Implemented]
 - List Based Parent/Child Structure [Implemented]
 - Taggable todo items. [Started]
 - Records changes in the state of the todo items. (Not Implemented Yet)
 - Database upgrades should be easy for the user (Not Implemented Yet)

## Installation Instructions

htodo can be installed pretty easily. Simply check out the code and:

    cd /path/to/htodo
    cabal update
    cabal install

And that is all that there is to it. Any configuration or setup will be asked of you inside the
program itself. (Also just make sure that ~/.cabal/bin, or its windows equivalent, is in your path.)

## Usage Instructions

For now all of the instructions that are avaliable can be gathered via the command line --help
option. For the general overview of what this program does please just do the following:

    $ htodo --help

And it will print out all of your options. From there it should be pretty obvious what does what.

However, if you still need a basic use case here is an example of using the most common features:

   $ htodo init
   $ htodo add
   comment> Add all of the computer parts to their own sub-todo list.
   priority> 4
   tags> 
   $ htodo add computer/parts
   comment> Buy some ram
   priority> 1
   tags> computer-part
   $ htodo add computer/parts
   comment> Buy a new hard drive.
   priority> 5
   tags> computer-part
   $ htodo show
   Main:
      1. Add all of the computer parts ot their own sub-todo list.

   computer:
      parts:
         2. Buy some ram
         3. Buy a new hard drive.
   $ htodo edit 2
   comment> Buy some really fast ram.
   priority> 1
   tags> computer-part
   $ htodo done 1
   comment> This task has been completed.
   $ htodo show
   Main:

   computer:
      parts:
         2. Buy some really fast ram.
         3. Buy a new hard drive.
   $

This use case should guide you through many of the common operations and make you feel confident that everything will "just work" the way that it should.

If you encounter any bugs then please send them to the GitHub bug tracker. If you want to contact me directly
then you will find my email address in the cabal file.
