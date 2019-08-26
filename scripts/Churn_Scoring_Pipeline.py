# Copyright 2017, 2018 IBM. IPLA licensed Sample Materials.
"""
Sample Materials, provided under license.
Licensed Materials - Property of IBM
© Copyright IBM Corp. 2019. All Rights Reserved.
US Government Users Restricted Rights - Use, duplication or disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
"""
import pandas as pd
from sklearn import model_selection
from sklearn.externals import joblib
import sys
import os, json
from collections import OrderedDict
import shap
from churn_prep import ChurnPrep


global model
global project_path
global explainer
model = None
project_path = None
explainer = None


def init():
    global model
    global project_path
    global explainer
    
    # set project_path
    project_path = os.environ.get("DSX_PROJECT_DIR")

    model_name = "CHURN_Model"
    version = "latest"
    model_parent_path = project_path + "/models/" + model_name + "/"
    metadata_path = model_parent_path + "metadata.json"

    # fetch info from metadata.json
    with open(metadata_path) as data_file:
        meta_data = json.load(data_file)

    # if latest version, find latest version from  metadata.json
    if (version == "latest"):
        version = meta_data.get("latestModelVersion")

    # prepare model path using model name and version
    model_path = model_parent_path + str(version) + "/model"

    # load model
    model = joblib.load(open(model_path, 'rb'))

    # initialize the SHAP explainer
    explainer = shap.TreeExplainer(model)
    

def score(args):
    global model
    global project_path
    global explainer
    
    # parse input arguments
    dataset_name = args.get("dataset_name")
    cust_id     = args.get("cust_id")
    sc_end_date  = args.get("sc_end_date")
    to_drop_corr = ['CUSTOMER_SUMMARY_FUNDS_UNDER_MANAGEMENT_mean', 'CUSTOMER_SUMMARY_FUNDS_UNDER_MANAGEMENT_min',
                'CUSTOMER_SUMMARY_FUNDS_UNDER_MANAGEMENT_max', 'CUSTOMER_SUMMARY_TOTAL_AMOUNT_OF_DEPOSITS_min',
                'CUSTOMER_SUMMARY_TOTAL_AMOUNT_OF_DEPOSITS_max', 'CUSTOMER_SUMMARY_TOTAL_AMOUNT_OF_DEPOSITS_sum',
                'CUSTOMER_ANNUAL_INCOME', 'CUSTOMER_NUMBER_OF_DEPENDENT_CHILDREN',
                'NUM_ACCOUNTS_WITH_RISK_TOLERANCE_MODERATE', 'NUM_ACCOUNTS_WITH_RISK_TOLERANCE_HIGH',
                'NUM_ACCOUNTS_WITH_RISK_TOLERANCE_VERY_LOW', 'NUM_ACCOUNTS_WITH_RISK_TOLERANCE_LOW']
    to_drop_more = ['CUSTOMER_TENURE', 'NUM_ACCOUNTS_WITH_INVESTMENT_OBJECTIVE_PLANNING',
                'NUM_ACCOUNTS_WITH_INVESTMENT_OBJECTIVE_SECURITY',
                'CUSTOMER_SUMMARY_TOTAL_AMOUNT_OF_DEPOSITS_max_min_ratio',
                'CUSTOMER_SUMMARY_TOTAL_AMOUNT_OF_DEPOSITS_current_vs_6_months_ago']

    to_drop = to_drop_corr + to_drop_more


#    to_drop      = args.get("to_drop")
    
    # load event data for selected cust_id
    dataset_path = project_path + "/datasets/" + dataset_name
    input_df = pd.read_csv(dataset_path, infer_datetime_format=True,
                     parse_dates=['CUSTOMER_RELATIONSHIP_START_DATE', 'CUSTOMER_SUMMARY_END_DATE', 'CUSTOMER_SUMMARY_START_DATE'])
    input_df = input_df[input_df['CUSTOMER_CUSTOMER_ID'] == cust_id]
    
    # the churn_prep.py scripts saves out the last user inputs used for prepping the data
    # import this dictionary and pass the variables to the prep function
    # this ensures that the inputs used for prepping the training data are the same as those used for prepping the scoring data
    user_inputs_dict = joblib.load(open(project_path + '/datasets/training_user_inputs.joblib', 'rb'))
    # convert the dictionary into all the variables required. The dictionary key becomes the variable name
    globals().update(user_inputs_dict)
    
    # prep the scoring data
    scoring_prep = ChurnPrep('score', effective_date=sc_end_date, feature_attributes=feature_attributes, derive_column_list=derive_column_list,
                       granularity_key=granularity_key, target_attribute=target_attribute, status_attribute=status_attribute,
                       funds_attribute=funds_attribute, date_customer_joined=date_customer_joined, 
                       customer_end_date=customer_end_date, customer_start_date=customer_start_date,
                       period_attribute=period_attribute, status_flag_churn=status_flag_churn, AUM_reduction_threshold=AUM_reduction_threshold,
                       forecast_horizon=forecast_horizon, observation_window=observation_window, sum_list=sum_list, cat_threshold=cat_threshold)
    prepped_data = scoring_prep.prep_data(input_df, 'score')

    # handle empty data
    if prepped_data.shape[0] == 0:
        print("Data prep filtered out customer data. Unable to score.", file=sys.stderr)
        return None

    # drop columns that were determined in model training
    prepped_data = prepped_data.drop(to_drop, axis=1)

    # predict on prepped data
    predictions = model.predict(prepped_data).tolist()
    classes = None
    probabilities = None
    try:
        if hasattr(model, 'classes_'):
            classes = model.classes_.tolist()
        if hasattr(model, 'predict_proba'):
            probabilities = model.predict_proba(prepped_data).tolist()
    except:
        pass
    
    impact_features, shap_plot_html, data, elem_id = getImpactFeatures(explainer, prepped_data, str(type(model)))

    return {
        "classes": classes,
        "probabilities": probabilities,
        "predictions": predictions,
        "explain": impact_features,
        "explain_plot_html": shap_plot_html,
        "explain_plot_data": data,
        "explain_plot_elem_id": elem_id
    }


# function to extract highest impact features from SHAP as dictionary
def getImpactFeatures(explainer, df, model_type):
    shap_values = explainer.shap_values(df)
    
    # account for multi-output models
    shap_input = shap_values
    expected_input = explainer.expected_value
    if model_type.endswith("sklearn.ensemble.forest.RandomForestClassifier'>"):
        shap_input = shap_values[0]
        expected_input = explainer.expected_value[0]
    
    # Select top and bottom 3 impact features
    sv_row = pd.DataFrame(shap_input, columns=df.columns.tolist()).iloc[0].sort_values()
    neg_impact = sv_row.head(3)
    pos_impact = sv_row.tail(3)
    
    # Get HTML of shap plot
    plot = shap.force_plot(expected_input, shap_input, df.iloc[0,:]).data
    
    # Extract HTML and JS components
    script_split = plot.split('<script>')
    plot_html = script_split[0].strip('\n ')
    plot_js = script_split[1].split('</script>')[0]
    data = plot_js.split('SHAP.AdditiveForceVisualizer, ')[1].split('),')[0]
    elem_id = plot_js.split("document.getElementById('")[1].split("')")[0]
    
    impact_features = neg_impact.append(pos_impact)
    return(impact_features.to_dict(), plot_html, data, elem_id)


def test_score(args):
    """Call this method to score in development."""
    init()
    return score(args)
