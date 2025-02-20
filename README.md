📊 Project Overview
This project explores customer purchasing behavior through an interactive R Shiny dashboard built using the grc.csv dataset. The dashboard offers deep insights into sales patterns, spending habits, and item associations—empowering data-driven decision-making for businesses.

✨ Key Questions Explored:

Which payment method is most popular among customers?
What’s the relationship between age and total spending?
Which cities generate the highest sales?
How are customers clustered based on spending and demographics?
What items are frequently bought together?
🗃️ Dataset
The grc.csv dataset includes:

💰 Payment Data (Cash/Credit)
👥 Customer Demographics (Age, City)
🛒 Purchase Details (Items Bought, Total Spending)

🧰 Features & Functionalities
🔍 1. Data Cleaning
Removed duplicates, handled missing values, and filtered out outliers.
Ensured accurate item listings and consistent data formats.
📈 2. Interactive Visualizations
Payment Type Distribution (Pie Chart)
Age vs. Total Spending (Bar Chart)
City-wise Spending (Ranked Bar Chart)
Spending Distribution (Histogram)
K-Means Clustering (PCA Visualization with Plotly)
🧮 3. Clustering & Association Rules
K-Means Clustering: Grouped customers by age and spending patterns.
Apriori Algorithm: Discovered relationships between items using user-defined support and confidence thresholds.

🛠️ Tech Stack
Language: R
Framework: Shiny
Libraries:
ggplot2, plotly — Data Visualization
arules, arulesViz — Association Rules
dplyr, stringr — Data Manipulation
DT — Interactive Data Tables

📂 How to Run the Project
Install Required Libraries:
R
Copy
Edit
install.packages(c("shiny", "shinythemes", "ggplot2", "dplyr", "stringr", 
                   "arules", "arulesViz", "plotly", "DT"))
Launch the App:
R
Copy
Edit
library(shiny)
runApp("path_to_project_folder")
Upload the grc.csv File via the dashboard.
Adjust Parameters for clustering and association rules, then explore the results!

💡 Insights & Takeaways
Cash payments dominate sales, but certain cities prefer credit.
Younger customers spend less on average compared to older demographics.
High-frequency item combinations highlight opportunities for cross-selling.
