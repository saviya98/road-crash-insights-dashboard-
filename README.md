# Road Crash Insights Dashboard: Enhancing Road Safety

An end-to-end data analytics project analysing a 200,000+ record traffic accident
dataset to identify the conditions that most increase the risk of a crash being
fatal or severe, and to translate those findings into a decision-support
dashboard for city planners, police, and public safety analysts.

## Dashboard Preview
![Road Crash Overview Dashboard](https://github.com/saviya98/road-crash-insights-dashboard-/blob/main/dashboard/Dashboard%20Preview.png)

## Business Problem

City authorities face ongoing pressure to reduce road deaths and serious
injuries (e.g. Wellington's "Road to Zero" strategy targets a 40% reduction by
2030). This project analyses a large traffic accident dataset to answer:

> **Which conditions most elevate the chance of a crash becoming fatal or
> incapacitating, and when should road safety resources be deployed?**

The resulting insights are intended to support decisions on policing hours,
lighting upgrades, and weather-responsive speed limits.

## Dataset

- **Source:** [Kaggle - Traffic Accidents dataset](https://www.kaggle.com/datasets/oktayrdeki/traffic-accidents?resource=download) (Ördekçi, 2025)
- **Size:** ~209,000 records, 24 variables
- **Variables:** crash type, weather condition, lighting condition, road
  surface condition, injury severity, number of units involved, and more

## Methodology

| Stage | Description |
|---|---|
| **1. Business problem & dataset rationale** | Defined the research problem and evaluated dataset suitability |
| **2. Data wrangling & EDA** | Cleaned and transformed the raw data in R (type conversion, deduplication, handling unknowns, feature engineering: `is_night`, `is_weekend`, `peak_hour`, `severity`), then explored crash patterns by time, weather, and road conditions |
| **3. Statistical inference** | Tested significance of risk factors using correlation analysis, Chi-Squared, and Mantel-Haenszel tests; built a baseline logistic regression model |
| **4. Clustering / machine learning** | Applied Gower distance clustering (mixed numeric/categorical data) across variables including `is_night`, `precip`, `is_weekend`, `hour`, `intersection`, `num_units`, `traffic_control_device`, and `crash_type` to group crashes into actionable risk profiles |
| **5. Dashboard & user manual** | Built an interactive Power BI dashboard and wrote a user manual for target audiences (city planners, police/traffic authorities, public safety analysts, policy makers) |

## Key Findings

- Crash volume peaks in the **3pm-6pm** window, and the fatal/severe
  percentage roughly doubles on **wet weekend nights after 8pm**
- **Single-vehicle loss-of-control** events dominate fatalities, while
  **multi-vehicle pile-ups** are more associated with minor injuries
- Night driving, wet road surfaces, and weekend travel emerged as the
  strongest independent drivers of fatal or severe outcomes (combined
  night-wet odds ratio of approximately 2, per Chi-Squared/Mantel-Haenszel
  testing)
- A baseline logistic regression model achieved ~62% accuracy, motivating the
  move to clustering methods to better capture non-linear risk patterns

## Repository Structure

```
road-crash-insights-dashboard/
├── README.md
├── scripts/
│   ├── 01_data_wrangling_and_eda.R      # Cleaning, feature engineering, EDA visualisations
│   └── 02_clustering_ml.R                # Gower distance clustering
├── reports/
│   ├── 01_business_problem_and_dataset_rationale.docx
│   ├── 02_data_wrangling_and_eda.docx
│   ├── 03_statistical_inference.docx
│   ├── 04_clustering_and_ml.docx
│   └── 05_dashboard_user_manual.docx
├── dashboard/
│   └── Road_Accident_Dashboard.pbix       # Power BI file (requires Power BI Desktop to open)
└── presentation/
    └── Road_Crash_Insights_Dashboard_Presentation.pptx
```

## Tools & Technologies

- **R** (dplyr, ggplot2, lubridate, stringr, forcats, naniar, GGally) - data
  wrangling, statistical testing, and exploratory visualisation
- **Power BI** - interactive dashboard for non-technical stakeholders
- **Statistical methods** - Pearson correlation, Chi-Squared test,
  Mantel-Haenszel test, logistic regression, Gower distance clustering


## Note on Viewing the Dashboard

GitHub cannot render `.pbix` files in the browser. To view the dashboard,
download `dashboard/Road_Accident_Dashboard.pbix` and open it in
[Power BI Desktop](https://www.microsoft.com/en-us/power-platform/products/power-bi/desktop) (free).

## Author

Savith Rangana Dissanayakage
[LinkedIn](https://www.linkedin.com/in/savithdissanayake/) | [GitHub](https://github.com/saviya98)
