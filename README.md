A front end for review (https://github.com/amanbhurji/review) written with Elm

Only loads a paste (without comments) for now.

You'll need an instance of the review server with a paste.
Replace `pasteId` in the `Http` section with a valid id and then run
`elm reactor` or `elm make src/Main.elm`.

To build a page -
`elm make src/NewPaste.elm --output=newpaste.js`
then create a html page importing the js file created above and 
any desired style sheet with a <div id="app"/> body content

Add this to the html file.
```
<script>
  var app = Elm.NewPaste.init({
    node: document.getElementById("app")
  });
</script>
```

Note: For applications created with Browser.document or
Browser.application you dont need to create the "app" div since their init
functions do not need a node to be passed in.
