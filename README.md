**Contacts for Support**
- @rupertlssmith on https://elmlang.slack.com
- @rupert on https://discourse.elm-lang.org

# Running the demo.

The reader should consult the AWS Cognito documentation in order to complete some preliminary steps which are:

1. Set up an AWS developer account.
2. Create a user pool in Cognito, and add a user to it.
3. Set up an application that can authenticate against this user pool.

Modify the user pool id and client id to match those that you set up above. These
are currently hard coded in `Main.init`.

Build and run the demo with:

    > npm install
    > npm start
