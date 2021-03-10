
#MGSC661 - Final Project
#Sophie Courtmanche-Martel & Duncan Wang



#######  Feature Selection Using Chi-Squared  ######### 

import pandas as pd

#import data
df = pd.read_csv('hospital_data.csv')

#drop invalid predictors that cannot be observed at the time of hospital visit 
df.drop(['num_callouts','num_diagnosis','num_procedures','num_CPT_events','num_inputs','num_labs','num_microlabs','num_notes','num_outputs','num_drugs','num_procedural_events','num_transfers','num_chart_events','expired_hospital','total_num_interactions'], axis = 1, inplace = True)

#we will perform a cross tabulation between all levels of all variables and all levels of the target
pd.crosstab(df['admit_type'], df['LOS_days'])

#perform chi-squared tests on categorical variables
categorical_columns = df.drop(['LOS_days','age'], axis = 1).columns
chi2_check = []
for i in categorical_columns:
    if chi2_contingency(pd.crosstab(df['LOS_days'], df[i]))[1] < 0.05:
        chi2_check.append('Reject Null Hypothesis')
    else:
        chi2_check.append('Fail to Reject Null Hypothesis')
res = pd.DataFrame(data = [categorical_columns, chi2_check] 
             ).T 
res.columns = ['Column', 'Hypothesis']

#perform bonferroni adjusted pair-wise comparisons as post-hoc tests to see which levels of the significant variables are related to LOS days 
#this produces the final list of categorical predictors
check = {}
for i in res[res['Hypothesis'] == 'Reject Null Hypothesis']['Column']:
    dummies = pd.get_dummies(df[i])
    bon_p_value = 0.05/df[i].nunique()
    for series in dummies:
        if chi2_contingency(pd.crosstab(df['LOS_days'], dummies[series]))[1] < bon_p_value:
            check['{}-{}'.format(i, series)] = 'Reject Null Hypothesis'
        else:
            check['{}-{}'.format(i, series)] = 'Fail to Reject Null Hypothesis'
res_chi = pd.DataFrame(data = [check.keys(), check.values()]).T
res_chi.columns = ['Pair', 'Hypothesis']

#create a new list with significant facotrs to subset original dataset by
significant_chi = []
for i in res_chi[res_chi['Hypothesis'] == 'Reject Null Hypothesis']['Pair']:
    significant_chi.append('{}_{}'.format(i.split('-')[0],i.split('-')[1]))


#dummify variables 
new_df = pd.get_dummies(data = df, columns = df.select_dtypes(exclude = 'number').columns)

#only subset those selected by the chi squared analysis 
dummified_vars = new_df[significant_chi]
dummified_vars['age'] = df['age']
dummified_vars['LOS_days'] = df['LOS_days']

#reformat columns 
dummified_vars.columns = dummified_vars.columns.str.strip().str.lower().str.replace(' ', '_')

#export the selected columns as a CSV 
dummified_vars.to_csv('hospital_data_factors.csv')

    
