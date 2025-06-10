# Executable Environment for OSF Project [mp9td](https://osf.io/mp9td/)

This repository was automatically generated as part of a project to test the reproducibility of open science projects hosted on the Open Science Framework (OSF).

**Project Title:** Investigating the Communication-Induced Memory Bias in the Context of Third-Party Social Interactions

**Project Description:**
> In this study we used a screen-based violation of expectation (VOE) paradigm to investigate whether 9-month-old infants process qualitatively different information about novel objects when an object is introduced in a third-party joint visual attention versus an observed non-joint social interaction context. In 12 test trial videos, an object was shown together with two adults (action phase). The interpersonal sharedness between the two actors was manipulated in a way that they either look at the object together after previous mutual eye contact or they look at it individually without sharing mutual eye contact at any point. After each video, the scene was occluded before it revealed one of three different outcomes: the object infants had just seen in the action phase (no change, baseline), the object they had just seen but at a novel position (location change), or a new object (identity change). By manipulating both the third-party joint attention context and features of the object in the subsequent outcome phase, we aimed to examine whether observed communicative context biases infants to encode surface features which support learning about object kinds (i.e., object identity) over spatial-temporal information (i.e., object location).

Note: In the overall study (published in Open Mind: https://doi.org/10.1162/opmi_a_00114), we refer to this project as “Experiment 2”. This OSF project is directly linked to the OSF project " Investigating the Role of Gaze Cues on the Communication-Induced Memory Bias" (https://osf.io/t4yqj/). 

**Original OSF Page:** [https://osf.io/mp9td/](https://osf.io/mp9td/)

---

**Important Note:** The contents of the `mp9td_src` folder were cloned from the OSF project on **12-03-2025**. Any changes made to the original OSF project after this date will not be reflected in this repository.

The `DESCRIPTION` file was automatically added to make this project Binder-ready. For more information on how R-based OSF projects are containerized, please refer to the `osf-to-binder` GitHub repository: [https://github.com/Code-Inspect/osf-to-binder](https://github.com/Code-Inspect/osf-to-binder)

## flowR Integration

This version of the repository has the **[flowR Addin](https://github.com/flowr-analysis/rstudio-addin-flowr)** preinstalled. flowR allows visual design and execution of data analysis workflows within RStudio, supporting better reproducibility and modular analysis pipelines.

To use flowR, open the project in RStudio and go to `Addins` > `flowR`.

## How to Launch:

**Launch in your Browser:**

🚀 **MyBinder:** [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/code-inspect-binder/osf_mp9td-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment in your web browser.
   * Please note that Binder may take a few minutes to build the environment.

🚀 **NFDI JupyterHub:** [![NFDI](https://nfdi-jupyter.de/images/nfdi_badge.svg)](https://hub.nfdi-jupyter.de/r2d/gh/code-inspect-binder/osf_mp9td-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment on the NFDI JupyterHub platform.

**Access Downloaded Data:**
The downloaded data from the OSF project is located in the `mp9td_src` folder.

## Run via Docker for Long-Term Reproducibility

In addition to launching this project using Binder or NFDI JupyterHub, you can reproduce the environment locally using Docker. This is especially useful for long-term access, offline use, or high-performance computing environments.

### Pull the Docker Image

```bash
docker pull meet261/repo2docker-mp9td-f:latest
```

### Launch RStudio Server

Run the container (with a name, e.g. `rstudio-dev`):
```bash
docker run -it --name rstudio-dev --platform linux/amd64 -p 8888:8787 --user root meet261/repo2docker-mp9td-f bash
```

Inside the container, start RStudio Server with no authentication:
```bash
/usr/lib/rstudio-server/bin/rserver --www-port 8787 --auth-none=1
```

Then, open your browser and go to: [http://localhost:8888](http://localhost:8888)

> **Note:** If you're running the container on a remote server (e.g., via SSH), replace `localhost` with your server's IP address.
> For example: `http://<your-server-ip>:8888`

## Looking for the Base Version?

For the original Binder-ready repository **without flowR**, visit:
[osf_mp9td](https://github.com/code-inspect-binder/osf_mp9td)

