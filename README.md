# Investigating-Headline-Sentiment
Investigating Headline Sentiment, Sensationalism, and Content Patterns in News Data


# INF6027 News Headline Analysis

This repository contains the code and resources for analyzing news headlines, focusing on sentiment, sensationalism, and content patterns.

---

## Project Overview
This project investigates:
- The distribution and patterns of headline types (Declarative, Sensationalist, Question-based).
- Emotional tone and sensationalism in headlines.
- Machine learning models for headline classification.

---

## Folder Structure
- **MN-DS-news-classification.csv**: Contains the dataset - download from link https://paperswithcode.com/dataset/mn-ds.
- **analysis.R**: All R scripts for analysis and modeling:
- **README.md**: Project instructions and setup.

---

## Prerequisites
- **R version**: 4.0 or higher
- **Required R packages**:
  - `tidyverse`
  - `tm`
  - `syuzhet`
  - `ggplot2`
  - `caret`
  - `e1071`
  - `wordcloud`

Install packages using:
```R
install.packages(c("tidyverse", "tm", "syuzhet", "ggplot2", "caret", "e1071", "wordcloud"))
