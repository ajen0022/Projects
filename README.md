# Heritage New Zealand Pouhere Taonga Membership Transformation Project 

## Introduction

Our goals focus on gaining insights into member perceptions, benefits, pricing, and engagement to guide organisational strategy while providing data-driven recommendations related to membership transformation.

In this Git repo, we have included the presentation report, the complete report, raw data, and analysis code. We organised and analysed the data using R, and based on our analysis results, we provided recommendations from different perspectives.

## Table of contents

- Structure of Git Repository

- Description of Git Repository

- How To Use Dashboard

- Authors

- Dataset

- License 

## Structure of Git Repository

```
├── HNZPT_membership_project.Rproj
├── LICENSE
├── Presentation.pptx
├── README.md
├── analysis
│   └── analysis.Rmd
├── app.R
├── data
│   ├── 2024_membership_data.xlsx
│   ├── data_combined.csv
│   ├── data_dictionary.xlsx
│   ├── session_1_keypoint.docx
│   ├── session_2_keypoint.docx
│   └── session_4_keypoint.docx
├── report
│   ├── AACSB.png
│   ├── AMBA.png
│   ├── EQUIS.png
│   ├── MBSportrait.jpg
│   ├── _extensions
│   │   └── quarto-monash
│   │       └── report
│   │           ├── AACSB.png
│   │           ├── AMBA.png
│   │           ├── EQUIS.png
│   │           ├── MBSportrait.jpg
│   │           ├── _extension.yml
│   │           ├── before-title.tex
│   │           ├── monash2.png
│   │           └── title.tex
│   ├── image
│   │   ├── newplot.png
│   │   └── wordcloud.png
│   ├── monash2.png
│   ├── references.bib
│   ├── report.pdf
│   └── report.qmd
└── rsconnect
    └── shinyapps.io
        └── ajen0022
            └── HNZPT_membership_project.dcf
```

## Description of Git Repository

- **/analysis/**

Stores all drafts and processes from our analyses, including R code for plots, modelling, and text notes summarising key insights from each session.

- **/rsconnect/ & app.R**

Holds the Shiny app script (app.R) used to create the interactive membership survey data dashboard.

- **/data/**

Stores all project data files, including the raw membership survey data (2024_membership_data.xlsx), its cleaned and processed version (data_combined.csv), a data dictionary (data_dictionary.xlsx) outlining fields in the cleaned dataset, and summarised key points from the focus group sessions(session_1_keypoint.docx, session_2_keypoint.docx, session_4_keypoint.docx).
 
- **/report/**

Contains the project report and related resources, including citation files (references.bib) and the final report in PDF and Quarto (report.qmd) formats.

- **HNZPT_membership_project_Rproj**

The R Project file facilitates an organised RStudio workspace for the project.

- **LICENSE**

The MIT License file specifies terms and conditions for the use, reproduction, and distribution of this project.

- **Presentation.pptx**

A PowerPoint presentation summarising key findings and insights from the project.

- **README.md**

This README file provides an overview of the project structure, usage instructions, and relevant information.


## How To Use Dashboard

**Option 1: Access via Link**

Simply click [here](https://ajen0022.shinyapps.io/HNZPT_membership_project/) to access the dashboard.

**Option 2: Run Locally via RStudio**

Install R and RStudio if they have not already been installed on your computer.

Open the app.R file located in the /dashboard/ directory in RStudio.

Click "Run App" in RStudio to launch the dashboard.


## Dataset

Our datasets belong to Heritage New Zealand Pouhere Taonga and include:

- **Focus Group Dataset:** Video recordings from three focus group sessions.

- **Survey Data:** A survey sent to all members to gather insights on various membership topics.


## Authors

Monash University, Master of Business Analytics

ETC5543 - Business analytics creative activity 

- Amrita Jena (ajen0022@student.monash.edu / ID: 33729166)

- Chih-Hui (Chloe) Wang (cwan0169@student.monash.edu / ID: 33209006)


## License 

This project is licensed under the MIT License. For more details, please refer to the LICENSE file.
