import os
import sys
import numpy as np
import pandas as pd 
import matplotlib.pyplot as plt

# Setting Up Environment Varible
PATH = r"C:\Users\malik\AppData\Local\Continuum\miniconda3\envs\data601\Lib\site-packages"
sys.path.append(PATH)

from statsmodels.stats.outliers_influence import variance_inflation_factor


# Where to save the figures
PROJECT_ROOT_DIR = "."
IMAGES_PATH = os.path.join(PROJECT_ROOT_DIR, "images")
os.makedirs(IMAGES_PATH, exist_ok=True)



def set_environemt_variable():
    PATH = r"C:\Users\malik\AppData\Local\Continuum\miniconda3\envs\data601\Lib\site-packages"
    sys.path.append(PATH)

def save_fig(fig_id, tight_layout=True, fig_extension="png", resolution=300):
    path = os.path.join(IMAGES_PATH, fig_id + "." + fig_extension)
    print("Saving figure", fig_id)
    if tight_layout:
        plt.tight_layout()
    plt.savefig(path, format=fig_extension, dpi=resolution)

    
def calculate_vif(variables):
    # we create a new data frame which will include all the VIFs
    # note that each variable has its own variance inflation factor as this measure is variable specific (not model specific)
    vif = pd.DataFrame()

    # here we make use of the variance_inflation_factor, which will basically output the respective VIFs 
    vif["VIF"] = [variance_inflation_factor(variables.values, i) for i in range(variables.shape[1])]
    # Finally, I like to include names so it is easier to explore the result
    vif["Features"] = variables.columns
    
    return vif