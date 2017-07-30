#### PurpleRug

PurpleRug is a parsing program that converts markdown files to html files.
To build the app locally, clone down the repository and run:

```
stack build
```

When executing the program, you must provide two arguments:
1. The markdown input file to be converted
2. The html output file where the resulting html will get written (with a `.html` extension)

For example:

```
stack exec purple-rug my-markdown-file.md my-output-file.html
```

Note: If the output file already exists, PurpleRug *will* overwrite that file

##### Tests

To run the tests, simple run:

```
stack test
```

##### Functionality

Right now the app really only operates on a few particular constructs. These constructs are:
- Paragraphs
- Headers
- Lists (Unordered and Ordered)
- Simple word formatting
  - em tags
  - strong tags


Feel free to add an issue if you'd like to see something new introduced!
