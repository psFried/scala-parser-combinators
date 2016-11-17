Parser Combinators in Scala
===========================


This is an example implementation of parser combinators in Scala. Parts of it are naive and amateurish, but serves the purpose of an example.

If you're unfamiar with the benefits of using parser combinators, then you should take a look at the [example json parser](src/main/scala/com/paxata/parsercombinators/Json.scala) that fits roughly in one screen height. It's really not a complete json parser implementation, but it does show how expressive and terse the code can be.

The code has some comments, so check out the source to learn more.

## Pants

This code is build using [Pants](http://www.pantsbuild.org). To run all the tests, run: `./pants test src/test/scala/com/paxata/parsercombinators`. Pants will bootstrap itself, so you don't need to install it. It just requires python 2.7 to be installed and on the PATH. 

