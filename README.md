# 🇨🇴 Colombian Fintech Customer Analytics – Website

This repository contains the **Quarto-based project website** for the ISSS608 Visual Analytics group project.

🔗 Live Website:  
https://elliolee.github.io/ISSS608_VAA_ColombianFintech-project-website/

---

## 📌 Project Overview

This project develops an **interactive visual analytics system** based on the COFINFAD dataset (48,723 customers, 57 variables) to support data-driven decision-making in a Colombian fintech context.

The analytical workflow follows a structured pipeline:

Customer Structure  
→ Customer Overview & Segmentation
→ Customer Engagement & Experience  
→ Customer Retention & Value  

---

## 🧱 Website Structure

The website is built using **Quarto** and organized into modular pages:
├── index.qmd                # Homepage (entry + storytelling)
├── project/
│   ├── proposal.qmd
│   └── meeting_minutes.qmd
├── modules/
│   ├── module_overview.qmd
│   ├── module_engagement.qmd
│   └── module_retention.qmd
├── poster.qmd
├── user_guide.qmd
├── shiny.qmd               # (optional landing page)
├── images/
├── styles.css
├── _quarto.yml
├── docs/                   # Rendered site (for GitHub Pages)

---

## 🔗 Integration with Shiny App

The website serves as the frontend navigation layer, while the Shiny app provides interactive analytics.

👉 Shiny App (hosted on shinyapps.io):
https://ligen.shinyapps.io/isss608_vaa_colombianfintech-project-shiny/
