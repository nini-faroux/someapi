<h2>some api</h2>

An JSON API which allows the user to register via email, authenticate 
and then interact with a protected resource via the use of JWT tokens.

Interact with a sample fly deployed app at:
  * https://some-api.fly.dev/

Example request for user registration:

```shell
curl -X POST 'https://some-api.fly.dev/user' \
-H 'Content-Type: application/json' \
-d '{ "name": "<username>", "email": "<emailAddress>", "password": "<password>"}'
```

Reference the documentation here:
  * https://nini-faroux.github.io/someapi/

***

To build and run locally with Docker, run the following:

```bash
docker-compose -f docker-compose-local.yml up --build
```

You can then interact with the API at:
  * localhost:8080

For docker-compose to work locally you will need to:
  * set up a gmail account
  * create an app password for that account 
  * create a '.env' file, setting the environment variables specified in the '.env.example' file.
  
These environment variables include the HMAC secret needed for the JWT tokens.

***

To run the integrations tests, run the following:

```bash
./setup-integration-test.sh
```

This will remove the associated (pgdata) volume before running the docker compose local setup.

Wait until it is ready to receive requests, then run the following:

```bash
stack test :integration-test
```

This will first create the required environment variables locally,
then run the tests, and then unset the environment variables before finishing.

***

To update the swagger documentation, run the following:

```bash
./update-docs.sh
```
