# TripadvisoR

DATA 534 Project

Collaborators:
- Kiran John
- Foster Lockerbie
- Seamus Riordan-Short

## Overview
The `TripadvisoR` package offers a suite of functions for the [Tripadvisor API](https://tripadvisor-content-api.readme.io/). Note that users are able to make 5,000 free monthly calls before being charged.

> "Increase your user experience, engagement, and conversion with Tripadvisor’s globally recognized and highly trusted content. Our partner API provides you with dynamic access to Tripadvisor content, enabling seamless integration with your website and applications. Locations are defined within this API as hotels, restaurants or attractions."

This R package utilizes the various APIs provided by Tripadvisor to retrieve:
- city information based on a string
- the top 10 locations based on a string, with the option of specifying coordinates to search from
- the 10 closest locations within proxmity of a specified attraction
- provide detailed information for a given location id

## Usage

To install and load `TripadvisoR`, use the following commands in RStudio:

`devtools::install_github("JKiran4/TripadvisoR")`
`library("TripadvisoR")`

Make sure you set the value of `api_key` in your RStudio environment to a valid [Tripadvisor API](https://www.tripadvisor.com/developers) key before running commands.

For an overview of package functionality use the following command: `help("TripadvisoR")`

![](img/tripadvisorowl.png)

