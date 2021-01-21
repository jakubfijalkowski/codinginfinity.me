+++
title = "Making Atlantis multitenant"
date = 2021-01-21T11:30:00+02:00
[taxonomies]
tags = ["devops", "rust", "tools"]
+++

I love Infrastructure as Code. I popularized it in the company that I work for - all of our projects now _start_ with the infrastructure codified. I'm also trying to move all of the company's _shared_ (e.g. self-hosted CI tools) infrastructure definitions to code. I've had my share of clicking on the web panels and writing CLI scripts to manage the cloud. I still think doing this manually is _much faster_ for quick fixes or some temporary changes but to make it sustainable, maintainable and discoverable you have to make it declarative - so codify it, as everything else!

My tool of choice for IaC is [Terraform](https://www.terraform.io/). There are other solutions available on the market (ARM, CloudFormation, [Pulumi](https://www.pulumi.com/), ...) but that doesn't really matter here now. The point is - as with everything - IaC is better when applied automatically. :slightly_smiling_face: And here comes Terraform Atlantis, a "pull request automation" (basically CI/CD :wink: ) for Terraform.

[Atlantis](https://www.runatlantis.io/) is a great tool, really, but it is _quite_ simple. It does one thing and does it well but it lacks "corporatey" things that I, unfortunately, need - multitenancy. But fear not! Atlantis can be scripted so we can easily add that ourselves on top of it. So welcome [kvenv](https://github.com/jakubfijalkowski/kvenv), a simple tool that allows you to juggle credentials in order to shield yourself from others (and probably yourself too :slightly_smiling_face:).

<!-- more -->

# Why do we need Atlantis in the first place

Terraform (or Pulumi for that matter) have a rather... peculiar way of doing changes. Don't get me wrong - this is fully justified, only that it is not intuitive if you come from a web/backend programmer background. Instead of just "building", you first plan your changes, see what the tool will change. Only after manual inspection (or automated tests for that matter, but testing IaC isn't that popular right now) you **apply** your changes. And the problem is that sometimes a single `apply` for a simple change won't suffice. Sometimes you have to stage the changes manually - the tools aren't perfect, the cloud isn't perfect and you just don't have any other way right now.

This is where Atlantis comes in. It is not your normal CI/CD system - it is one of the few tools that do it in a ChatOps-y way. Instead of just planning on every push or applying everything on merge to `main`, it allows you to, step by step, instruct it how it should do the apply dance. Only after the changes are fully applied, merge the PR. Consider a simple Azure-based Terraform code (simplified):

```terraform
# Snip...

resource "azurerm_kubernetes_cluster" "example" {
  name                = "example-aks1"
  kubernetes_version  = "1.18.10"

  # Snip...
}

resource "azurerm_kubernetes_cluster_node_pool" "example" {
  name                  = "internal"
  kubernetes_cluster_id = azurerm_kubernetes_cluster.example.id
  orchestrator_version  = "1.19.3"

  # Snip...
}
```

This alone will generate a plan that creates both a cluster and then an additional node pool. It won't however tell you that it is wrong - you can't have a node pool with version higher than the control plane. Only after applying you yoill find out about that. If we were using a typical CI/CD workflow (deploy/apply on merge), it would be too late - the branch is merged, you have to do the fix in other PR. Atlantis, after checking the initial plan, would allow you to leave

```sh
atlantis apply
```

as a comment. It will then proceed and apply the previous plan. Everything _before_ the merge.

This isn't perfect. I would much more prefer to have it follow the trunk-based development with normal CI/CD. Without all the fuss. Unfortunately that's not really possible right now - the cloud and thus the applies are just too flaky. Atlantis is a tool that acknowledges that. Acknowledges and gives you a way to mitigate the effects. With a little bit of manual work but everything has its price. When the time comes, we would be able to use "normal" way of doing things. Right now we have to live with that and improve the tooling along the way.

We will get there one day :slightly_smiling_face:

# The god account problem

CI/CD for infrastructure has a slight problem - the usual deployment favors a god account, also known as a **master account**. That is, the account that has read/write access to everything. That owns everything. If it gets compromised, the attacker can do anything - steal your data, steal your money or plant a backdoor in your delivery pipeline.

This is rather undesirable.

You can't really get rid of the master account. If your organization is _centralized_, which might or might not be a good idea, splitting everything into small parts is just not feasible. Also, even if you split your responsibilities cleanly, you will have the same problem but on lower levels (and probably a slightly lower scale). You can however make it a little bit more secure - by making Atlantis multitenant and use multiple credentials.

{{ figure(src="hierarchical_keys.svg", width="30%", caption="Fig. 1 - multiple keys") }}

What does that mean? As you can see in figure 1, you still have the master account. However, this time the master account can't really **do** anything. It won't be able to change the real infrastructure. It will only be used to get another set of credentials (`K1`/`K2`) - the ones that are able to access and change the corresponding parts of infrastructure (`R1`/`R2`).

This does not solve the problem completely - if the attacker gains the access to the master account, it can access each and every set of real credentials. It will take much more time to perform the attack, but it doesn't prevent it. We need to go one step further and introduce more... physical walls.

{{ figure(src="atlantis_vnet.svg", width="40%", caption="Fig. 2 - physical access limit") }}

The master credentials should be used only by the Atlantis instance - this means that we can completely block external access to the credential store. Atlantis, being a hosted solution, will probably run on a VM inside a VNet (or VPC or whatever it's called in your Cloud of Choice). This means we can use normal networking solutions, like firewalls or network filtering and make it accessible from within the VNet (or even the VM) only. This means that if the attacker has the master credentials but does not have direct access to the VM/VNet, they can do nothing.

At our company we use Azure most of the time. That's why I'm the most fluent in their services (and that's why the above images use Azure icons :slightly_smiling_face: ). Nevertheless, the aforementioned trick might be successfully used in other clouds as well.

### Azure
On Azure, this might be accomplished with a number of AAD Applications, each one having roles assigned to a different part of the infrastructure (resources or resource groups, depending how granular access you want to have). You can store all of the necessary credentials in the Azure KeyVault as secrets and limit access to the KV with networking ACLs (to only a single IP/subnet) or private links with NSGs. You then designate a single application to be the "master" one and give it read (and only read!) access to the Key Vault (where the credentials are stored).

## Preparing credentials for Terraform
Having the idea on how to distribute the credentials, we might ask ourselves how to store them. Let's start by asking a question - how do we want to use them?

Most of the tools want the credentials to be passed either in interactive prompt, through a config file, with normal arguments or through environment variables. Fortunately for us, Terraform and all the providers allow every combination of the options. We can select whatever works for us, but in my opinion, environment variables are the most universal - they can be passed as parameters, `echo`ed to config files or redirected to the interactive prompts. That's why we might start with them and try to put our secrets there.

This isn't really novel - Azure allows you to get environment variables out of Key Vault if you use [App Services](https://docs.microsoft.com/en-us/azure/app-service/app-service-key-vault-references). Unfortunately, there isn't really any way to make KV secrets available as environment variables outside Azure environment.

Or is it?

# Welcome `kvenv`
I haven't found any, [so I made one](https://github.com/jakubfijalkowski/kvenv). :slightly_smiling_face: It really is a rather simple tool (although I have some ideas for improvements) that allows you to download an environment definition from Azure Key Vault and then run your program in that environment (merged with the normal process env). Simple idea, rather simple solution.

## Environments in `kvenv`
KeyVault is already quite popular as a configuration source for [ASP.NET Core apps](https://docs.microsoft.com/en-us/aspnet/core/security/key-vault-configuration?view=aspnetcore-5.0#secret-storage-in-the-production-environment-with-azure-key-vault). There, it uses "secret per configuration entry" approach, where each secret translates directly to a single value. This is an OK-ish approach, but it can add quite some execution time - you need to download each secret value separately and KeyVault isn't a speed demon (for good reasons). You can expect a couple hundred milliseconds latency for each `get`, which results in quite a substantial delay. That's why [`kvenv`](https://github.com/jakubfijalkowski/kvenv) uses a different approach - it prefers to store the whole environment in one secret.

How? By storing the env in JSON. :slightly_smiling_face: It adds a little bit of overhead, but for our usage it should be negligible (KV limits secret size to 25 KB). The JSON is just a flat dictionary with keys being variable names and values being... values. It currently does not support complex objects but that might come in the future. Example:

```json
{
    "VARIABLE_A": "Value 1",
    "VARIABLE_2": 12.3
}
```

will result in an environment with two variables: `VARIABLE_A` and `VARIABLE_2`. I think you get the idea. :slightly_smiling_face:

## Using `kvenv`
Having an environment stored in KeyVault, we can use the master account to access the secret. Since I want to use `kvenv` with Atlantis and the only way we can pass secret values there is with the process environment variables (or files, but that would be rather... nasty), I assumed that being able to use these would be beneficial. That's why `kvenv` can get the whole config both from parameters as well as environment variables (`KVENV_*`):
* `KVENV_TENANT_ID`,
* `KVENV_CLIENT_ID`,
* `KVENV_CLIENT_SECRET`,
* `KVENV_KEYVAULT_NAME`, and
* `KVENV_SECRET_NAME`.

Thanks to [Clap](https://github.com/clap-rs/) you can freely mix and match them, whatever suits you best.

So, let's assume that we already have the necessary variables set. Then simply running

```sh
$ kvenv run-in -- echo '$VARIABLE_A'
Value 1
$ kvenv run-in -- env
...
VARIABLE_A="Value 1"
VARIABLE_1="12.3"
...
```

will allow you to run arbitrary programs in a modified environment. `kvenv` merges the custom environment from KV with the current system environment so that you might still use the master credentials in the subprogram. That's why `kvenv` also supports [masking](https://github.com/jakubfijalkowski/kvenv#masking) - you can specify variable names which **cannot** be visible to the child program. If the child does not see it, it does not exist, doesn't it?. :slightly_smiling_face:

# Integrating it all together

Now that we have all the building blocks ready, we can start building the platform.

## Credentials

First, we need to decide how to scope (that is - who can use them) the child credentials. In my opinion, making them scoped to a repository level (or anything repository-related, like projects inside a repository) is a good starting point. **This means that a whole infrastructure in a single repository will be managed with a single AAD application**. Let's assume that we can stringify the scope name according to KV rules. Having that, we can store the tenant (AD application credentials to be exact) credentials in a secret named after the scope.

Let's assume that we are working on a project that is stored in a repository named `atlantismultitenancy`. To use AzureRM/AzureAD Terraform providers, we need to run `terraform` in an environment like this (values are random):
```json
{
    "ARM_TENANT_ID": "1d36b7a3-2405-4ff9-97ce-f09f16385572",
    "ARM_CLIENT_ID": "f170e04e-11b2-4c85-add7-f5c6742a6154",
    "ARM_CLIENT_SECRET": "1c795fad-573d-45e0-b3df-051feebe2b02",

    "ARM_SKIP_CREDENTIALS_VALIDATION": true,
    "ARM_SKIP_PROVIDER_REGISTRATION": true
}
```
The last two values enable us to work with AzureRM provider [without AAD global admin](https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs#skip_credentials_validation).

Having this ready, we only need to load the configuration before calling `terraform`.

And for that, we need the master account. This is the part that I will partly omit. You need to inject the AAD App credentials and KV name as environment variables to the Atlantis instance. This highly depends on the deployment model. For example, for Kubernetes, you might store the credentials as a K8s secret and then load the env from it (either directly with `envFrom: secretRef:` or with `loadEnvFromSecrets` variable in the [Helm chart](https://github.com/runatlantis/helm-charts)). From now on I assume that the Atlantis does see the following variables (again, random data):
```sh
KVENV_TENANT_ID="ca44d7e2-ea61-44ce-a441-f2b2f0fecf3c"
KVENV_CLIENT_ID="acc23526-404b-41eb-866a-ec522b966b11"
KVENV_CLIENT_SECRET="a7acbd67-bc5b-4bd3-b494-3ef8b7f3a9af"
KVENV_KEYVAULT_NAME="kvname" # This can be hardcoded elsewhere, but having it in env will make our lives easier.
```

This is the last fragment of sensitive data that we need. We now have all the credentials accessible to Atlantis, the next step is to use them.

<small>But let's not leak them.</small>

## Workflow

The above solution has one limitation: we cannot allow custom workflows on repo-level. If we did allow that, every repo would be able to circumvent the `kvenv` masking and get the master account credentials without any problems (just put `run: echo $KVENV_CLIENT_SECRET` anywhere in the script and you're good to go). We need to have them defined centrally and with a limited write access (although they can be public), hence we need to define them in the [server-side repo config](https://www.runatlantis.io/docs/server-side-repo-config.html).

The workflow isn't really that different from the default one. You just need to run `init`, `plan` and `apply` using `kvenv` like this:
```sh
kvenv run-in -n "$HEAD_REPO_NAME" --mask KVENV_CLIENT_SECRET -- terraform ACTION
```
and we're done. The whole `default` workflow looks like this:
```yaml
workflows:
  default:
    plan:
      steps:
      - run: kvenv run-in -n "$HEAD_REPO_NAME" --mask KVENV_CLIENT_SECRET -- terraform init -no-color
      - run: kvenv run-in -n "$HEAD_REPO_NAME" --mask KVENV_CLIENT_SECRET -- terraform plan -no-color -out $PLANFILE
    apply:
      steps:
      - run: kvenv run-in -n "$HEAD_REPO_NAME" --mask KVENV_CLIENT_SECRET -- terraform init -no-color
      - run: kvenv run-in -n "$HEAD_REPO_NAME" --mask KVENV_CLIENT_SECRET -- terraform apply -no-color $PLANFILE
```

You can easily augment this with additional data, more secrets, different scoping rules or even introduce your own authorization. You can add Terragrunt or any other tool on top of that. I think this can also be used (with minor adjustments) for Pulumi. You can even prepare multiple workflows and allow overriding `workflow` in repo-level configuration. You just can't allow them to override `workflows`. :slightly_smiling_face:

### Side note on pre workflow hooks

Starting with version 0.16, Atlantis supports [`pre_workflow_hooks`](https://www.runatlantis.io/docs/pre-workflow-hooks.html) in server-side repo config. These could be used to run `kvenv`, prepare the environment and mask sensitive variables. The unfortunate fact is that it does not support changing environment variables right now and we really need that. So, until it supports it (maybe I will add that...? Finally I would have some real motivation to learn Go :slightly_smiling_face: ), the predefined workflows must do.

# Summary

Atlantis is a really neat tool. This was one of the few self-hosted tools that I deployed without much of a problem. Part of that is it's simplicity. It might be viewed as a flaw but that's not true - you have to build something on top of it to fully suit your needs. You can do so exactly because it is simple and it does not impose _that much_ on you.

This in turn allows you to use it as a core of your _platform_ for Terraform CI/CD. You have to design it well but with a little bit of tinkering you can build a multitenant solution for many separate users.

The approach, with all the apps and hierarchy and credentials and glue code, might not be the easiest one to grasp nor implement. It is however quite flexible, allowing you to almost freely assign permissions without giving too much. [`kvenv`](https://github.com/jakubfijalkowski/kvenv) helps here, but it is not the only approach for sure.

If you find this solution useful or I missed something (which I'm quite confident I did) or just that the approach does not make any sense - please let me know in the comments!




