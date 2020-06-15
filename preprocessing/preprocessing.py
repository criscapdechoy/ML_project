#!/usr/bin/env python
# coding: utf-8

import pandas as pd
import numpy as np


use_columns = ['code',
               'countries_en',
               'additives_n',
               'nutriscore_score',
               'nutriscore_grade',
               'nova_group',
               'pnns_groups_2',
               'fat_100g',
               'carbohydrates_100g',
               'sugars_100g',
               'fiber_100g',
               'proteins_100g',
               'salt_100g']



data = pd.read_csv('en.openfoodfacts.org.products.csv',
                   sep='\t',
                   low_memory=False,
                   usecols=use_columns,
                   index_col='code')

# Fix nutriscore
clean_data = data[data['nutriscore_score'].notnull() & data['countries_en'].notnull()]


# Fix the countries
country_lists = clean_data['countries_en'].str.split(',')
country_counts = country_lists.explode().value_counts()
top_countries = country_counts[country_counts > 1000]

use_countries = top_countries[top_countries > 1000].index.tolist()
clean_data = clean_data[clean_data['countries_en'].isin(use_countries)]

# Replace unknown and empy values with NA
clean_data = clean_data.replace(to_replace=['unknown',''], value=np.nan)


# Write result
clean_data.to_csv('clean_data.csv', sep=';', na_rep='NA')
