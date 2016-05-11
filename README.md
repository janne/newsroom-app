Newsroom.app
============

Setup
-----
* Install prerequisites

    ```
    npm install -g elm
    npm install
    ```

* Export your Mynewsdesk API key to environment variable MND_KEY

    ```
    export MND_KEY=foobar123
    ```

* Compile the Elm code

    ```
    elm-make src/Main.elm --output elm.js
    ```

* Run electron

    ```
    npm start
    ```
