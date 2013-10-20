HsApp

A generic command-line Haskell application

This a template application featuring common cmd-line parsing and checking routines designed to free your code from doing
boring things like checking arguments, and dealing with files and directory just before it starts doing usefull things.

The basic facility starts with get-otp parsing the command line and then the checking part that relies on clever threading 
of a base options record along the list of get-opt supplied functions in a monadic fashion. We use a EitherT monad on top 
of IO allowing us to perform efects while checking user supplied args.