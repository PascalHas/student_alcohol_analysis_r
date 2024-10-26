# Data Analysis and Machine Learning Project

This repository contains the project work for the course "Data Analysis and Machine Learning" at the University of Applied Science (FHDW) Bielefeld. The project includes a `.qmd` (Quarto Markdown) file and the corresponding `.r` script, which perform a specific data analysis and machine learning task as part of the course requirements.

## Installation

To run the code provided in this repository, you will need to have both R and RStudio installed on your computer. Follow the instructions below to set up your environment:

### Install R

R is a programming language and free software environment for statistical computing and graphics.

1. Visit the [R Project website](https://cran.r-project.org/).
2. Download the appropriate installer for your operating system (Windows, macOS, or Linux).
3. Run the installer and follow the on-screen instructions to complete the installation.

### Install RStudio

RStudio is an integrated development environment (IDE) for R.

1. Visit the [RStudio website](https://posit.co/download/rstudio-desktop/).
2. Download the RStudio Desktop installer for your operating system.
3. Run the installer and follow the on-screen instructions to complete the installation.

## Usage

### Create dataset path
In order to use the .qmd or .r Files you need to download the dataset from [Kaggle](https://www.kaggle.com/datasets/uciml/student-alcohol-consumption/data) and put it into a subfolder of the repository.
So your structure has to look like: student_alcohol_analysis_r/dataset/student_mat.csv and student_alcohol_analysis_r/dataset/student_por.csv.

### Importing and Running the `.qmd` File

The `.qmd` (Quarto Markdown) file contains the main documentation and code chunks for the analysis. Quarto is an open-source scientific and technical publishing system that allows for dynamic, reproducible documents.

1. Open RStudio.
2. Install Quarto by following the [installation instructions](https://quarto.org/docs/get-started/).
3. Open the `.qmd` file in RStudio.
4. Click on the "Render" button to execute the code and generate the output document (e.g., HTML, PDF).

### Running the `.r` File

The `.r` file contains the R script necessary for the analysis. To run it:

1. Open RStudio.
2. Open the `.r` file in RStudio.
3. Run the code by clicking "Run" or using the keyboard shortcut (`Ctrl` + `Enter` for Windows/Linux, `Cmd` + `Enter` for macOS) to execute individual lines or code blocks.

## Additional Information

If you encounter any issues or have questions, please feel free to reach out to me or use the RStudio community forums for support.

## Feedback and Summary from Lecturer

In this data analysis project, several strengths and opportunities for refinement were identified. While the topic and approach aligned well with the course scope, the analysis could benefit from clearer structure, particularly in presenting and evaluating hypotheses. The regression problem should have been introduced at the start, instead of emerging gradually.

Although exploratory data analysis (EDA) techniques, such as boxplots and scatter plots, were correctly utilized, some interpretations were lacking—particularly in explaining summary statistics and justifying the initial variable selection for the model, which lacked correlation justification. Residuals and cross-validation methods were applied but required a clearer explanation for their inclusion. Repeated "summary" displays were deemed excessive, and data merging could have been visualized more effectively with intermediary plots.

**Final Grade: 2.0**
