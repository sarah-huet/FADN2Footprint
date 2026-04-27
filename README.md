# FADN2Footprint: Agricultural practices and Environmental footprints from FADN data

 ██     ██  █████  ██████  ███    ██ ██ ███    ██  ██████
 ██     ██ ██   ██ ██   ██ ████   ██ ██ ████   ██ ██
 ██  █  ██ ███████ ██████  ██ ██  ██ ██ ██ ██  ██ ██   ███
 ██ ███ ██ ██   ██ ██   ██ ██  ██ ██ ██ ██  ██ ██ ██    ██
  ███ ███  ██   ██ ██   ██ ██   ████ ██ ██   ████  ██████
  
=================================================================

FADN2Footprint - DEVELOPMENT VERSION

=================================================================

This package has been validated ONLY for:
- Cereals farms (TF14 == 15)
- Dairy farms   (TF14 == 45)

Only FADN versions post 2014 are supported.

Please filter your FADN data to these farm types and years before use.
Results for other farm types are UNTESTED and may be unreliable.

=================================================================

## Table of contents

-   [General infos](#general-infos)
-   [Intallation](#installation)
-   [Data available](#data-available)
-   [Peer-reviewed articles about
    FADN2Footprint](#peer-reviewed-articles-about-fadn2footprint)
-   [Other resources](#other-resources)

![FADN2Footprint](vignettes/logo.jpg)

## General infos {#general-infos}

FADN2Footprint is an R library that transforms Farm Accountancy Data
Network (FADN) records into comprehensive sustainability assessments.
Estimates farm practices and computes environmental footprints (GHG
emissions, biodiversity, water), economic performance, and social
indicators at multiple scales (country, NUTS2, farm, activity, product)
using life cycle assessment principles and Scope 3 accounting.

## Installation {#installation}

This R package was built using R 4.5.0 and depends on R \>= xxx

**Step 1: Install the devtools package**

`install.packages("devtools")`

**Step 2: Install FADN2Footprint**

`library(devtools)`

`install_github("sarah-huet/FADN2Footprint")`

**Step 3: Load FADN2Footprint**

`library(FADN2Footprint)`

## Data available {#data-available}

A mock FADN data set can be loaded from the FADN2Footprint package to
test it.

## Peer-reviewed articles about FADN2Footprint {#peer-reviewed-articles-about-fadn2footprint}

Huet, Ayouba & Bellassen (in prep.) *FADN2Footprint: an R package to
infer agricultural practices and compute farms environmental footprints
from FADN data*

## Other resources {#other-resources}

The FADN2Footprint project also has a number of supporting online
resources, most of which can by found at [the FADN2Footprint home
page](https://sarah-huet.github.io/fadn2footprint.html).

To post feature requests or ask for help, try [the FADN2Footprint Issue
Tracker](https://github.com/sarah-huet/FADN2Footprint/issues).
