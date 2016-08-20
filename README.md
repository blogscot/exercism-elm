# exercism-elm
My Elm language Exercism solutions

## Notes

As Elm is a transpile-to-JS language, testing the Exercism exercises is a little more involved compared 
to other languages I've tried.

To get started

- npm install -g elm-test
- cd into the exercise directory
- run elm-test init
- copy the exercism test file and solution file into tests/
- rename the test file to Main.elm
- modify the line: main : Program Never -> main : Program Value

Now from the exercise directory, type elm-test to run the tests.
