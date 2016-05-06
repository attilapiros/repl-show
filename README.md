
# repl-show
a Clojure clone of the [marconilanna](https://github.com/marconilanna/REPLesent)

## Best feature

You can execute the source examples which are just displayed (syntax highlighted) in your slide (to be precise in the current build of the current slide).

### Example 

Before execution when a slide with source is displayed:

![Before execution](/doc/13.PNG)

After the execution with *(run)*: 

![After execution](/doc/13_run.PNG)

## How to learn it

- clone the repo

- start a repl: 
```
lein repl
```

- use the repl-show namespace:
```
 (use 'repl-show.core)
```

- start the tutorial which is 15 easy slides about the markup and navigation: 
```
(start)
```

## How to use it for your presentation

- create a text file with your super presentation content, you can create it for example in the project root 

- start a repl

- use the repl-show namespace:
```
 (use 'repl-show.core)
```

- start the presentation: 
```
(start "path-to-the-presentation-file")
```

- enjoy :)
