**This is Guillermo Gallardo class project repository**

# Instructions

To replicate this project, navigate to the products/manuscript directory and open the Manuscript.qmd file. Click on *Render* to process the project and generate the Word document. Below, you'll find a breakdown of the various folders containing sources, code, images, and tables. Ensure that R is installed on your computer, and having some knowledge of how to clone repositories will simplify the process.


# Template structure and content


* All code goes into the `code` folder and subfolders. Currently, there are 3 sub-folders that do different parts of an analysis. he first folder, named *eda-code*, is dedicated to exploratory data analysis and includes tables and plots created using the HRprocesseddata.rds file. The second folder, *processing-code*, focuses on data cleaning processes. The third folder, *analysis-code*, contains the code for the project's analysis phase.

* All data goes into the `data` folder and subfolders. Currently, there are 2 sub-folders that contain the HR dataset used for this project. The first folder is called *raw-data* and it contains the raw data, including an Excel file named "IBM HR Analytics Data." The second folder is called *processed-data* and it houses the processed data in an RDS format, with the file named HRprocesseddata. This file is extensively used throughout the project for data manipulation and exploratory data analysis (EDA).

* The `products` folder contains the main file that is used to create this project. In this folder, you will find the *manuscript* file, which you can render to export a Word document containing all the data and plots.

* The `results` folder contains automatically/code generated output. This includes figures, tables saved as serialized R data (`.Rds`) files, computed values and other outputs. All content in these folders should be automatically generated by code. Manually generated results should be avoided as much as possible.


* There are multiple special files in the repo.
  * `readme.md`: this file contains instructions or details about the folder it
  is located in. You are reading the project-level `README.md` file right now. There is a `readme` in almost every folder.
  * `HR-Turnover-Analysis.Rproj` is a file that tells RStudio that this is the main folder for a project. Rename if you want.

