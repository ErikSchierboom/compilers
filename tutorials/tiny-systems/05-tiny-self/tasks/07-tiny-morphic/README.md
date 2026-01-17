\# Tiny Morphic demo



This project is using the Fable F# to JavaScript compiler. See \[Fable website](https://fable.io/docs/getting-started/setup.html) for setup instructions. You will need to install .NET SDK, Node.js and npm.



To start the project, run:



```

dotnet restore

npm install

```



Then you can actually run it:



```

npm run start

```



This is a script defined in `package.json` which runs Fable with `watch` parameter (so that changes to code are automatically recompiled) and runs `vite` which will serve the compiled web page:



```

dotnet fable watch --verbose --run npx vite

```



Vite will start a web server at http://localhost:5173/

