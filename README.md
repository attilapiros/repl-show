# under development

## repl-show
a Clojure clone of the [marconilanna](https://github.com/marconilanna/REPLesent)


## How to use it

- clone the repo
- create a text file with the your super presentation content, you can create it for example in the project root 
- start a repl: 
```
lein repl
```
- use the repl-show.core namespace: 
```
(use '[repl-show.core :as show])
```
- start the presentation: 
```
(show/start "path-to-the-presentation-file")
```
- enjoy
