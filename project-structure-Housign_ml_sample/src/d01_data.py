import os
import tarfile
import urllib
import pandas as pd 
import numpy as np
from sklearn.model_selection import StratifiedShuffleSplit



DOWNLOAD_ROOT = "https://raw.githubusercontent.com/ageron/handson-ml2/master/"
HOUSING_PATH = os.path.join("datasets", "housing")
HOUSING_URL = DOWNLOAD_ROOT + "datasets/housing/housing.tgz"

def download_housing_data(housing_url=HOUSING_URL, housing_path=HOUSING_PATH):
    if not os.path.isdir(housing_path):
        os.makedirs(housing_path)
    tgz_path = os.path.join(housing_path, "housing.tgz")
    urllib.request.urlretrieve(housing_url, tgz_path)
    housing_tgz = tarfile.open(tgz_path)
    housing_tgz.extractall(path=housing_path)
    housing_tgz.close()
    

def load_housing_data_as_DF(housing_path=HOUSING_PATH):
    csv_path = os.path.join(housing_path, "housing.csv")
    return pd.read_csv(csv_path)

def create_income_categories(housing: pd.DataFrame):
    return pd.cut(housing["median_income"],
                                  bins=[0,1.5,3,4.5,6,np.inf],
                                  labels=[1,2,3,4,5])


def split_Stratified_Split_train_test(housing: pd.DataFrame):
    split = StratifiedShuffleSplit(n_splits=1, test_size=0.2, random_state=42)
    for train_index, test_index in split.split(housing, housing["income_cat"]):
        strat_train_set = housing.loc[train_index]
        strat_test_set = housing.loc[test_index]
    return strat_train_set,strat_test_set


def drop_icome_category_varibale_from_train_n_test(housing_train,housing_test):
    if "icome_cat" in housing_train: 
        housing_train.drop("income_cat",axis=1, inplace=True)
    if "icome_cat" in housing_test: 
        housing_test.drop("income_cat",axis=1, inplace=True)

        
MAIN_PATH = os.path.join("datasets","raw_splited")
TRAIN_FILE_PATH = os.path.join(MAIN_PATH,"housing_train.csv")
TEST_FILE_PATH = os.path.join(MAIN_PATH,"housing_test.csv")

def save_housing_train_n_test_datasets_CSV(housing_train,housing_test):
    if not os.path.isdir(MAIN_PATH):
        os.makedirs(MAIN_PATH)
    housing_train.to_csv(TRAIN_FILE_PATH ,sep=',', encoding='utf-8',index=False)
    housing_test.to_csv(TEST_FILE_PATH ,sep=',', encoding='utf-8',index=False)
    
def load_housing_train_DF():
    return pd.read_csv(TRAIN_FILE_PATH)

def load_housing_test_DF():
    return  pd.read_csv(TEST_FILE_PATH)