In order to embed Elm apps in react, you must:
1. Install react-elm-components from Github, NPM runs a little behind and a recent change 
broke in recent react apps
`yarn add evancz/react-elm-components`
2. Install elm webpack loader
`yarn add elm-webpack-loader --save-dev`
3. yarn eject (this moves all configuration to project root, allowing you to configure webpack)
4. In both conf/webpack.config.dev.js and conf/webpack.config.prod.js
Tell webpack to resolve .elm file
```
extensions: ['.js', '.json', '.jsx', '.elm'], // add .elm to the list of extensions to resolve
```

Ask the file loader to ignore .elm files
```
{
	exclude: [
	  /\.html$/,
	  /\.(js|jsx)$/,
	  /\.css$/,
	  /\.json$/,
	  /\.bmp$/,
	  /\.gif$/,
	  /\.jpe?g$/,
	  /\.png$/,
	  /\.elm$/, // Ask the file loader to ignore .elm files
	],
	loader: require.resolve('file-loader'),
	options: {
	  name: 'static/media/[name].[hash:8].[ext]',
	},
},
```

Register the elm-webpack-loader to handle .elm files
```
// loader for elm files
{
	test: /\.elm$/,
	exclude: [/elm-stuff/, /node_modules/],
	loader: require.resolve('elm-webpack-loader')
},
```

Finally, elm-stuff and elm-package.json should be at the same level as package.json, which means if you're migrating
an existing elm app into this react app, you put the .elm files in src/, but move the elm-package.json file up, and
ensure the src directories in elm-package.json are pathed to where your elm files are relative to the project root. 
