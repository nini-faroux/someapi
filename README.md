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
  * https://some-api.fly.dev/docs

***

To build and run locally with Docker, run the following:

```bash
docker-compose -f docker-compose-local.yml up --build
```

You can then access the API at:
  * localhost:8080

For docker-compose to work locally you will need to:
  * set up a gmail account
  * create an app password for that account 
  * create a '.env' file, setting the environment variables specified in the '.env.example' file.
  
These environment variables include the HMAC secret needed for the JWT tokens.

***

To update the swagger documentation, run the following:

```bash
stack build
stack exec -- some-api-exe -d
```
