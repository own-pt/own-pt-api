Notes
=====

- Automatic deployment was disabled because we would need to check-in
  a manifest.yml with the private API key.  Besides, we were hitting
  some sort of hard limit on the number of deployments per month on
  Bluemix anyway.

- Make sure you copy manifest-template.yml to manifest.yml and fill
  out the missing information about the environment (the private API
  key).

- To get the value of the private API key before the deployment, you
  simply run "cf e ownpt2" and copy the value of the API_KEY
  environment.

- If you don't know the private API key, and cannot find it at all,
  you need to generate a new one and update CL-WNBROWSER accordingly
  (see SECRETS*.LISP there).

- Do NOT check-in the manifest.yml with the private API key!

- To actually deploy, just issue a "cf push ownpt2" from this
  directory (but see next bullet item before doing so).

- To deploy with zero downtime, read up on Blue Green deployments.
  See the deploy.sh file for an example of a Blue Green deployment.
  Essentially you rename the current application, deploy a new one
  with the old name, and then delete the renamed application.

- If you are deploying with Blue Green deployment, make sure you have
  enough memory available on the system for BOTH applications.
