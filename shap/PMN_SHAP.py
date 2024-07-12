import pandas as pd
import numpy as np
import shap
import fasttreeshap
from sklearn.ensemble import RandomForestRegressor
import matplotlib.pyplot as plt

# Constants
classProperty = 'MeanPMN'
df = pd.read_csv('/Users/johanvandenhoogen/ETH/Projects/costa_rica/shap/averagedmodellingdata_forSHAP.csv')


# Rename variables in covariateList to increase readability
envCovariateList = [
    'Ann_Precip', 'EVI', 'Elevation', 'Human.footprint',
       'AvgCanopyHeight', 'nearest_10',
]

# One hot encode type_vars
df = pd.get_dummies(df, columns=['Type'], dtype = 'int')

# Get type_vars
type_vars = [col for col in df.columns if 'Type' in col]

# Create final list of covariates
covariateList = envCovariateList + type_vars 

# Subset columns from df
df = df[covariateList + [classProperty]]

df.head()

# Set categorical variables
for cat in type_vars:
    df[cat] = df[cat].astype('category')

# Load data and labels
X = df[covariateList]
y = df[classProperty]

# Train Random Forest models and calculate SHAP values

hyperparameters = {
    'n_estimators': 250,
    'min_samples_split': 2,
    'max_features': None,
    'max_samples': 0.632,
    'random_state': 42
}

# Create 
classifier = RandomForestRegressor()
classifier.set_params(**hyperparameters)

classifier.fit(X, y)

explainer = fasttreeshap.TreeExplainer(classifier)

shap_values = explainer(df[covariateList])
shap_values = shap_values.values

# # Read SHAP values from file
# with np.load('shap_values_AM.npz') as data:
#       shap_values_list = [data[f'arr_{i}'] for i in range(len(data.keys()))]

# Plot 1: SHAP summary plot, with all features
plt.figure()
shap.summary_plot(shap_values, pd.DataFrame(data=df, columns=covariateList), show = False, sort = True)
plt.xlabel('Mean absolute SHAP value')
plt.tight_layout()
# plt.show()
plt.savefig('shap_summary_plots_full.png', dpi=300)

# Plot 2: SHAP summary plot, with type_vars removed

# Get the indices of the features to drop
drop_indices = [i for i, feat in enumerate(covariateList) if feat in type_vars]

# Create a mask where only the features not in type_vars are True
mask = np.ones(len(covariateList), dtype=bool)
mask[drop_indices] = False

# Create a new dataframe without the features to drop
df_filtered = df[envCovariateList]

# Filter the mean SHAP values
shap_values_filtered = shap_values[:, mask]

# Plot and save figure to file
plt.figure()
shap.summary_plot(shap_values_filtered, df_filtered, show=False, sort=True)
plt.xlabel('Mean absolute SHAP value')
plt.tight_layout()
# plt.show()
plt.savefig('shap_summary_plots_typeRemoved.png', dpi=300)

# Plot 3: SHAP summary plot, with type_vars grouped together
# Sum 'type_vars' SHAP values together
type_shap_values = np.sum(shap_values[:, len(covariateList) - len(type_vars):], axis=1).reshape(-1, 1)

# Get SHAP values for other features
other_shap_values = shap_values[:, :len(covariateList) - len(type_vars)]

# Combine 'type_vars' SHAP values with other features
combined_shap_values = np.hstack([other_shap_values, type_shap_values])

# Create new feature names list
new_feature_names = envCovariateList + ["type_vars"]

# Create a df where project vars are Nan
df_type_vars_grouped = df[envCovariateList]
df_type_vars_grouped.loc[:, 'Type'] = np.NaN

plt.figure()
shap.summary_plot(combined_shap_values, features = df_type_vars_grouped, sort=True, show = False)
plt.xlabel('Mean absolute SHAP value')
plt.tight_layout()
# plt.show()
plt.savefig('shap_summary_plots_typeGrouped.png', dpi=300)

# # Plot 4: SHAP dependence plots for the top 6 features
# # Create SHAP explanation object        
# explanation = shap.Explanation(values=shap_values_filtered,
#             # base_values=shap_values_list[0].base_values,
#             data=pd.DataFrame(data=df[envCovariateListRenamed + ['amf_sampling_density'] + [classProperty]], columns=envCovariateListRenamed + [classProperty]),
#             feature_names=list(df[envCovariateListRenamed + ['amf_sampling_density'] + [classProperty]].columns))

# # Get the top 6 most important features
# importance = np.abs(explanation.values).mean(0)
# top_6 = np.argsort(-importance)[:6]

# # Create a multipanelled figure of the top 6 features
# fig, axes = plt.subplots(nrows=2, ncols=3, figsize=(15, 8))

# # Plot
# for i, feature_idx in enumerate(top_6):
#     shap.dependence_plot(list(envCovariateListRenamed+ ['amf_sampling_density'])[feature_idx], explanation.values, X[envCovariateListRenamed + ['amf_sampling_density']], ax=axes[i // 3, i % 3], interaction_index = 'auto', show=False)
#     plt.tight_layout()

# # Save figure to file
# plt.savefig('figures/20240620_arbuscular_mycorrhizal_rwr_shap_scatter_plots_wInteraction.png', dpi=300)

# # Plot 5: SHAP dependence plots for the top 6 features, without interaction
# # Create a multipanelled figure of the top 6 features
# fig, axes = plt.subplots(nrows=2, ncols=3, figsize=(15, 8))

# # Plots without interaction
# for i, feature_idx in enumerate(top_6):
#     shap.dependence_plot(list(envCovariateListRenamed+ ['amf_sampling_density'])[feature_idx], explanation.values, X[envCovariateListRenamed + ['amf_sampling_density']], ax=axes[i // 3, i % 3], interaction_index = None, show=False)
#     plt.tight_layout()

# # Save figure to file
# plt.savefig('figures/20240620_arbuscular_mycorrhizal_rwr_shap_scatter_plots.png', dpi=300)

# # Plot 6: SHAP bar plot for the top 12 features, with project_vars grouped together
# plt.figure()
# shap.summary_plot(combined_shap_values, features = df_project_vars_grouped, plot_type = 'bar', sort=True, show = False, max_display=12)
# plt.xlabel('Mean absolute SHAP value')
# plt.tight_layout()
# # plt.show()
# plt.savefig('figures/20240620_arbuscular_mycorrhizal_rwr_shap_bar_plots_projectGrouped.png', dpi=300)

# shap_values = np.mean(np.abs(combined_shap_values), axis=0)

# # Create a DataFrame with feature names from df_project_vars_grouped and their corresponding mean SHAP values
# df_shap_values = pd.DataFrame({
#     'Feature': df_project_vars_grouped.columns,
#     'Mean SHAP Value': shap_values
# })

# # Sort by absolute mean SHAP value
# df_shap_values = df_shap_values.reindex(df_shap_values['Mean SHAP Value'].sort_values(ascending=False).index)

# # Write to file
# df_shap_values.to_csv('output/20240620_arbuscular_mycorrhizal_rwr_shap_values.csv', index=False)
