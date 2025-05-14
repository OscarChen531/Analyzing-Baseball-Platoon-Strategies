# CSAS2025 Data Challenge Submission

## 📊 Project Title
**Analyzing Baseball Platoon Strategies: Integrating Bat Speed, Swing Length, Spray Angle, Clustering, and Performance Metrics**

## 📝 Description
This project was developed for the **2025 Connecticut Sports Analytics Symposium (CSAS2025) Data Challenge**. It explores platoon strategies in Major League Baseball using newly introduced swing metrics from Statcast (2024), such as:

- **Bat Speed**
- **Swing Length**
- **Spray Angle**
- **Attack Angle**

We clustered batters based on swing profiles and simulated game outcomes using a **Monte Carlo simulation** to optimize lineup decisions. The project also examines how these strategies relate to **injury prevention** and **load management** in sports medicine.

## 📁 Repository Structure

csas2025-batter-analysis/
├── data/
│ └── [Processed datasets here]
├── figures/
│ └── [Plots and diagrams from the analysis]
├── notebooks/
│ └── platoon_code.R # Main analysis script (see reproducibility below)
├── final_wcc.pdf # Final project report submitted to CSAS2025
├── README.md # You're here
└── LICENSE # MIT License (can be changed as needed)


## 📦 Requirements & Setup

This project was written in **R**. To reproduce the analysis:

1. Clone this repository:
    ```bash
    git clone https://github.com/your-username/csas2025-batter-analysis.git
    cd csas2025-batter-analysis
    ```

2. Open `platoon_code.R` or load it into an RStudio project.

3. Install necessary packages:
    ```r
    install.packages(c("tidyverse", "data.table", "ggplot2", "cluster", "factoextra", "summarytools"))
    ```

4. Run the script:
    ```r
    source("notebooks/platoon_code.R")
    ```

## 📈 Summary of Methods

- **Data Source**: `statcast_pitch_swing_data_20240402_20241030_with_arm_angle.csv` provided by CSAS2025
- **Feature Engineering**:
  - Spray angle via `atan2()`
  - Attack angle from top 20% of swings
- **Dimensionality Reduction**: Principal Component Analysis (PCA)
- **Clustering**: K-means (separately for LHB and RHB)
- **Simulation**: Monte Carlo approach to simulate 10,000 9-inning games
- **Performance Metrics**: BA, OBP, SLG, OPS, OPS+

## 🧠 Key Findings

- **LHB vs RHP**: OPS+ = 108
- **RHB vs LHP**: OPS+ = 104
- Simulation average: **4.8664 runs/game** using a mixed platoon lineup
- Distinct batter profiles revealed via clustering (e.g., power vs contact hitters)

## ✅ Ethics Statement (CSAS2025)

This work adheres strictly to the CSAS2025 Data Challenge ethical guidelines:

### 🔒 Data Privacy
- No personal identifiable information (PII) beyond player names was used.
- Results are aggregated (e.g., by cluster) to avoid singling out players.

### ⚖️ Fairness and Integrity
- All statistical methods (PCA, clustering, simulations) were applied objectively.
- Both regular season and playoff data were included to reduce context bias.

### 🔍 Transparency and Reproducibility
- All preprocessing, modeling, and simulation steps are documented in `platoon_code.R`.
- Final report (`final_wcc.pdf`) details all methods used.

### 🙌 Responsible Use
- Findings are intended to enhance decision-making, not stigmatize individual players.
- We caution against overreliance on platoon matchups in player development decisions.

### 🏥 Injury & Welfare
Future work may incorporate injury data:
- **Confidentiality**: Aggregate, anonymized metrics only
- **Consent**: Required for medical data use
- **Purpose**: To support safer player workloads and reduce injury risk

## 📚 References
- Statcast 2024 data and visualizations (MLB.com)
- Winston et al. (2022). *Mathletics*
- Abdi & Williams (2010). PCA overview
- Petriello (2024). Statcast bat tracking documentation

## 📜 License
This repository is licensed under the [MIT License](LICENSE).

## 🙏 Acknowledgments
Special thanks to:
- **CSAS 2025 Organizers** for the opportunity
- **Statcast / MLB** for providing swing tracking data
- **R community** for supporting reproducible analysis tools

---

*Contact: Wei-Chieh Chen | Syracuse University | Applied Statistics Graduate Program*
