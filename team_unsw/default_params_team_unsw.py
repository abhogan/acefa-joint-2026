## Fixed parameters for the ACEFA model
import pandas as pd
import numpy as np

state_names = ["SU", "E1U", "E2U", "IU", "RU",
               "SV", "E1V", "E2V", "IV", "RV",
               "SW", "E1W", "E2W", "IW", "RW"]
               # "CaseIU", "CaseIV", "CaseIW"

age_names = ['0-5M', '6-11M', '1-4', '5-11', '12-17', '18-49', '50-64', '65-79', '80+']
acefa_ages = ['<1', '1-4', '5-11', '12-17', '18-64', '65-79', '>80']

SU, E1U, E2U, IU, RU = range(5)
SV, E1V, E2V, IV, RV = range(5, 10)
SW, E1W, E2W, IW, RW = range(10, 15)
CaseIU, CaseIV, CaseIW = range(15, 18)  ## infection counters

def load_population_australia(sheet_name='AUS'):
    ''' ABS: population clock and pyramid 2024 (by yearly age)
    https://www.abs.gov.au/statistics/people/population/population-clock-pyramid

    Population pyramid for Australia 2024 https://www.populationpyramid.net/australia/2024/
    returns dataframe of Australian population by age groups and percentages
    '''

    df = pd.read_excel('data/population_australia_2024_abs_australia.xlsx',  sheet_name=str(sheet_name.upper()))

    p = df.copy()
    total_population = np.sum(p["Male"]) + np.sum(p["Female"])

    p.insert(3, "Percent", (p["Male"] + p["Female"]) / total_population, True)

    C = np.array([p["Percent"][0] * .5,  # 0 to 5 months
                  p["Percent"][0] * .5,  # 6 to 11 months
                  p["Percent"][1:5].sum(),  # 1 to 4Y
                  p["Percent"][5:12].sum(),  # 5 to 11Y
                  p["Percent"][12:18].sum(),  # 12 to 17Y
                  p["Percent"][18:50].sum(),  # 18 to 49Y
                  p["Percent"][50:65].sum(),  # 50 to 64Y
                  p["Percent"][65:80].sum(),  # 65 to 79Y
                  p["Percent"][80:].sum()  # 80Y +
                  ])

    data = {"age": ['0-5M', '6-11M', '1-4', '5-11', '12-17', '18-49', '50-64', '65-79', '80+'],
            "proportion": C}

    df2 = pd.DataFrame(data)
    return df2

def load_population_australia_preschool(sheet_name='AUS'):
    ''' ABS: population clock and pyramid 2024 (by yearly age)
    https://www.abs.gov.au/statistics/people/population/population-clock-pyramid


    Population pyramid for Australia 2024 https://www.populationpyramid.net/australia/2024/
    returns dataframe of Australian population by age groups and percentages
    '''

    df = pd.read_excel('data/population_australia_2024_abs_australia.xlsx',  sheet_name=str(sheet_name.upper()))

    p = df.copy()
    total_population = np.sum(p["Male"]) + np.sum(p["Female"])

    p.insert(3, "Percent", (p["Male"] + p["Female"]) / total_population, True)

    ## 10 age groups
    C = np.array([p["Percent"][0] * .5,  # 0 to 5 months
                  p["Percent"][0] * .5,  # 6 to 11 months
                  p["Percent"][1].sum(),  # 1
                  p["Percent"][2:5].sum(),  # 2-4
                  p["Percent"][5:12].sum(),  # 5 to 11Y
                  p["Percent"][12:18].sum(),  # 12 to 17Y
                  p["Percent"][18:50].sum(),  # 18 to 49Y
                  p["Percent"][50:65].sum(),  # 50 to 64Y
                  p["Percent"][65:80].sum(),  # 65 to 79Y
                  p["Percent"][80:].sum()  # 80Y +
                  ])

    data = {"age": ['0-5M', '6-11M', '1', '2-4', '5-11', '12-17', '18-49', '50-64', '65-79', '80+'],
        "proportion": C}

    df2 = pd.DataFrame(data)
    return df2


def load_hospitalisation_rates_aihw(n_ages=9):
    ''' (Used for ACEFA report) reads hospitalisation data from 2023 from AIHW (national)'''

    H = np.array([399.9505216,  ## 0-0.5
                  399.9505216,  ## 0.5 - 1
                  269.4936615,  ## 1-4
                  163.437793,  ## 5-11
                  63.37041598,  ## 12-17
                  48.81885497,  ## 18-49
                  66.0036071,  ## 50-64
                  110.9720428,  ## 65-79
                  234.3497955  ## 80+
                  ])

    if n_ages==10:
        H = np.insert(H, 3, 269.4936615)
    return H

def load_death_rates(n_ages=9):
    ''' death rates per 100K population'''

    D = np.array([0.32642010,  ## 0-0.5
                  0.32642010,  ## 0.5 - 1
                  0.63790812,  ## 1-4
                  0.28372769,  ## 5-11
                  0.14413879,  ## 12-17
                  0.33810898,  ## 18-49
                  1.30117614,  ## 50-64
                  4.09704333,  ## 65-79
                  17.55042521  ## 80+
                  ])

    ## D(all ages) = 1.73684755

    if n_ages == 10:
        D = np.insert(D, 3, 0.63790812)

    return D

def load_reduced_susceptibility(n_ages=9):
    ''' For adults (18Y +), r = 0.21 (Hill2009)
    For children (0-18), r grows linearly by year (assumed)'''
    temp = np.linspace(0, 0.21, 19)[1:]
    reduced_sus = np.array([temp[0] * 0.25,  ## 0-5 month
                            temp[0] * 0.75,  ## 6-11 month
                            np.mean(temp[:4]),  ## 1-4Y
                            np.mean(temp[4:11]),  ## 5-11Y
                            np.mean(temp[11:17]),  ## 12-17
                            0.21, 0.21, 0.21, 0.21  ## 18+
                            ])
    if n_ages==10:
        reduced_sus[2] = temp[1]  ## age: 1 year
        reduced_sus = np.insert(reduced_sus, 3, np.mean(temp[1:4])) ## age: 2-4 year

    return reduced_sus



## Load VE and VC for 9 age groups
def load_VE_and_VC_IIV(vaccine_policy='status_quo', vaccine_scenario='pessimistic'):
    ''' for each vaccine policy (status_quo, laiv_5_12, laiv_5_18) and
    each vaccine scenario (status_quo, pessimistic, mid, optimistic):
    loads the vaccine coverage and vaccine effectiveness.

    In this function, VC is only IIV.

    'status_quo' vaccine policy will give status quo VE and VC (regardless of vaccine scenario)
    '''

    n_ages = 9

    if vaccine_scenario not in ['status_quo', 'pessimistic', 'mid', 'optimistic']:
        print('Warning: vaccine_scenario is not status_quo, pessimistic, mid, or optimistic.')
    if vaccine_policy not in ['status_quo', 'laiv_5_12', 'laiv_5_18']:
        print('Warning: vaccine_policy is not status_quo, laiv_5_12, or laiv_5_18.')

    #### Vaccine coverage by vaccine scenario: status quo, pessimistic, mid, optimistic
    ## LAIV 5 to 12:
    vc12 = np.array([[0.15, 0.15, 0.15, 0.10, 0.10, 0.20, 0.20, 0.55, 0.55],  ## standard
                     [0.15, 0.15, 0.15, 0.40, 0.10, 0.20, 0.20, 0.55, 0.55],  ## low (same as standard)
                     [0.15, 0.15, 0.15, 0.60, 0.10, 0.20, 0.20, 0.55, 0.55],  ## middle
                     [0.15, 0.15, 0.15, 0.80, 0.10, 0.20, 0.20, 0.55, 0.55]])  ## high

    ## LAIV 5 to 18:
    vc18 = np.array([[0.15, 0.15, 0.15, 0.10, 0.10, 0.20, 0.20, 0.55, 0.55],  ## standard
                     [0.15, 0.15, 0.15, 0.40, 0.40, 0.20, 0.20, 0.55, 0.55],  ## low (same as standard)
                     [0.15, 0.15, 0.15, 0.60, 0.60, 0.20, 0.20, 0.55, 0.55],  ## middle
                     [0.15, 0.15, 0.15, 0.80, 0.80, 0.20, 0.20, 0.55, 0.55]])  ## high

    ## Turn the 5-12s and 12-18s to 0 because they will be getting the LAIV
    vc12[1:, 3] = np.zeros(3)
    vc18[1:, 3] = np.zeros(3)
    vc18[1:, 4] = np.zeros(3)

    vc_of_vaccine_policy = vc12
    if vaccine_policy.lower() == 'laiv_5_18':
        vc_of_vaccine_policy = vc18

    ve_infection_laiv = np.array([0.44 * np.ones(n_ages),  ## standard
                                  0.44 * np.ones(n_ages),  ## low
                                  0.56 * np.ones(n_ages),  ## middle
                                  0.75 * np.ones(n_ages)])  ## high

    ve_gp_laiv = np.array([0.55 * np.ones(n_ages),  ## standard
                           0.55 * np.ones(n_ages),  ## low
                           0.65 * np.ones(n_ages),  ## middle
                           0.80 * np.ones(n_ages)])  ## high

    ve_hosp_laiv = np.copy(ve_gp_laiv)  ## same as ve_gp

    ## Vaccine effectiveness by the chosen vaccine policy and vaccine scenario: initiate with status quo scenario
    ve_inf_main = 0.44 * np.ones(n_ages)
    ve_gp_main = 0.55 * np.ones(n_ages)
    ve_hosp_main = np.copy(ve_gp_main)

    ## Assign an index by policy and age so we can update the ve_[inf/gp/hosp]_main accordingly by vaccine_policy
    my_policy_index = 0  ## status quo
    if vaccine_policy.lower() != 'status_quo':
        if vaccine_scenario.lower() == 'pessimistic':
            my_policy_index = 1
        elif vaccine_scenario.lower() == 'mid':
            my_policy_index = 2
        elif vaccine_scenario.lower() == 'optimistic':
            my_policy_index = 3

    my_age_index = []  ## status quo
    if vaccine_policy.lower() != 'status_quo':
        if vaccine_policy.lower() == 'laiv_5_12':
            my_age_index = [3]  ## 5-11 YO
        elif vaccine_policy.lower() == 'laiv_5_18':
            my_age_index = [3, 4]  ## 5-17 YO

    ## update the main VE arrays to policy program
    for i in my_age_index:
        ve_inf_main[i] = ve_infection_laiv[my_policy_index, i]
        ve_gp_main[i] = ve_gp_laiv[my_policy_index, i]
        ve_hosp_main[i] = ve_hosp_laiv[my_policy_index, i]

    vaccine_coverage = vc_of_vaccine_policy[my_policy_index]

    return [vaccine_coverage, ve_inf_main, ve_gp_main, ve_hosp_main]

def load_VE_and_VC_LAIV(vaccine_policy='status_quo', vaccine_scenario='pessimistic'):
    ''' for each vaccine policy (status_quo, laiv_5_12, laiv_5_18) and
    each vaccine scenario (status_quo, pessimistic, middle, optimistic):
    loads the vaccine coverage and vaccine effectiveness.

    'status_quo' vaccine policy will give status quo VE and VC (regardless of vaccine scenario)
    '''

    n_ages = 9

    if vaccine_scenario not in ['status_quo', 'pessimistic', 'mid', 'optimistic']:
        print('Warning: vaccine_scenario is not status_quo, pessimistic, mid, or optimistic.')
    if vaccine_policy not in ['status_quo', 'laiv_5_12', 'laiv_5_18']:
        print('Warning: vaccine_policy is not status_quo, laiv_5_12, or laiv_5_18.')

    #### Vaccine coverage by vaccine scenario: status quo, pessimistic, mid, optimistic
    ## LAIV 5 to 12:
    vc12 = np.zeros((4, n_ages))
    vc12[:, 3] = np.array([0, 0.40, 0.60, 0.80])  ## standard, low, middle, high


    ## LAIV 5 to 18:
    vc18 = np.zeros((4, n_ages))
    vc18[:, 3] = np.array([0, 0.40, 0.60, 0.80])  ## 5-11: standard, low, middle, high
    vc18[:, 4] = np.array([0, 0.40, 0.60, 0.80])  ## 12-17: standard, low, middle, high

    vc_of_vaccine_policy = vc12
    if vaccine_policy.lower() == 'laiv_5_18':
        vc_of_vaccine_policy = vc18

    ve_infection_laiv = np.array([0.44 * np.ones(n_ages),  ## standard
                                  0.44 * np.ones(n_ages),  ## low
                                  0.56 * np.ones(n_ages),  ## middle
                                  0.75 * np.ones(n_ages)]) ## high

    ve_gp_laiv = np.array([0.55 * np.ones(n_ages),  ## standard
                           0.55 * np.ones(n_ages),  ## low
                           0.65 * np.ones(n_ages),  ## middle
                           0.80 * np.ones(n_ages)]) ## high

    ve_hosp_laiv = np.copy(ve_gp_laiv)  ## same as ve_gp

    ## Vaccine effectiveness by the chosen vaccine policy and vaccine scenario: initiate with status quo scenario
    ve_inf_main = 0.44 * np.ones(n_ages)
    ve_gp_main = 0.55 * np.ones(n_ages)
    ve_hosp_main = np.copy(ve_gp_main)

    ## Assign an index by policy and age so we can update the ve_[inf/gp/hosp]_main accordingly by vaccine_policy
    my_policy_index = 0  ## status quo
    if vaccine_policy.lower() != 'status_quo':
        if vaccine_scenario.lower() == 'pessimistic':
            my_policy_index = 1
        elif vaccine_scenario.lower() == 'mid':
            my_policy_index = 2
        elif vaccine_scenario.lower() == 'optimistic':
            my_policy_index = 3

    my_age_index = []  ## status quo
    if vaccine_policy.lower() != 'status_quo':
        if vaccine_policy.lower() == 'laiv_5_12':
            my_age_index = [3]  ## 5-11 YO
        elif vaccine_policy.lower() == 'laiv_5_18':
            my_age_index = [3, 4]  ## 5-17 YO

    ## update the main VE arrays to policy program
    for i in my_age_index:
        ve_inf_main[i] = ve_infection_laiv[my_policy_index, i]
        ve_gp_main[i] = ve_gp_laiv[my_policy_index, i]
        ve_hosp_main[i] = ve_hosp_laiv[my_policy_index, i]

    vaccine_coverage = vc_of_vaccine_policy[my_policy_index]

    return [vaccine_coverage, ve_inf_main, ve_gp_main, ve_hosp_main]

## Load VE and VC for 10 age groups
def load_VE_and_VC_IIV2(vaccine_policy='status_quo', vaccine_scenario='pessimistic'):
    ''' for each vaccine policy (status_quo, laiv_2_4 laiv_2_12, laiv_2_18) and
    each vaccine scenario (status_quo, pessimistic, mid, optimistic):
    loads the vaccine coverage and vaccine effectiveness.

    In this function, VC is only IIV.

    The 2-4 age groups receives the LAIV vaccine at the same distribution speed as IIV but quicker (in half the duration)

    'status_quo' vaccine policy will give status quo VE and VC (regardless of vaccine scenario)
    '''

    n_ages = 10

    if vaccine_scenario not in ['status_quo', 'pessimistic', 'mid', 'optimistic']:
        print('Warning: vaccine_scenario is not status_quo, pessimistic, mid, or optimistic.')
    if vaccine_policy not in ['status_quo', 'laiv_2_4', 'laiv_2_12', 'laiv_2_18']:
        print('Warning: vaccine_policy is not status_quo, laiv_2_4, laiv_2_12, or laiv_2_18.')

    #### Vaccine coverage by vaccine scenario: status quo, pessimistic, mid, optimistic
    ## LAIV 2 to 4:
    vc4  = np.array([[0.15, 0.15, 0.15, 0.15, 0.10, 0.10, 0.20, 0.20, 0.55, 0.55],  ## standard
                     [0.15, 0.15, 0.15, 0.40, 0.10, 0.10, 0.20, 0.20, 0.55, 0.55],  ## low (same as standard)
                     [0.15, 0.15, 0.15, 0.40, 0.10, 0.10, 0.20, 0.20, 0.55, 0.55],  ## middle
                     [0.15, 0.15, 0.15, 0.40, 0.10, 0.10, 0.20, 0.20, 0.55, 0.55]])  ## high

    ## LAIV 2 to 12:
    vc12 = np.array([[0.15, 0.15, 0.15, 0.15, 0.10, 0.10, 0.20, 0.20, 0.55, 0.55],  ## standard
                     [0.15, 0.15, 0.15, 0.40, 0.40, 0.10, 0.20, 0.20, 0.55, 0.55],  ## low (same as standard)
                     [0.15, 0.15, 0.15, 0.40, 0.60, 0.10, 0.20, 0.20, 0.55, 0.55],  ## middle
                     [0.15, 0.15, 0.15, 0.40, 0.80, 0.10, 0.20, 0.20, 0.55, 0.55]])  ## high

    ## LAIV 2 to 18:
    vc18 = np.array([[0.15, 0.15, 0.15, 0.15, 0.10, 0.10, 0.20, 0.20, 0.55, 0.55],  ## standard
                     [0.15, 0.15, 0.15, 0.40, 0.40, 0.40, 0.20, 0.20, 0.55, 0.55],  ## low (same as standard)
                     [0.15, 0.15, 0.15, 0.40, 0.60, 0.60, 0.20, 0.20, 0.55, 0.55],  ## middle
                     [0.15, 0.15, 0.15, 0.40, 0.80, 0.80, 0.20, 0.20, 0.55, 0.55]])  ## high

    ## Turn the 5-12s and 12-18s to 0 because they will be getting the LAIV
    # vc4[1:, 3] = 0
    vc12[1:, 4:5] = 0
    vc18[1:, 4:6] = 0

    vc_of_vaccine_policy = vc4
    if vaccine_policy.lower() == 'laiv_2_12':
        vc_of_vaccine_policy = vc12
    if vaccine_policy.lower() == 'laiv_2_18':
        vc_of_vaccine_policy = vc18

    #### Vaccine effectiveness by vaccine scenario: status quo, pessimistic, mid, optimistic
    ## laiv
    ve_infection_laiv = np.array([0.44 * np.ones(n_ages),  ## standard
                                  0.44 * np.ones(n_ages),  ## low
                                  0.56 * np.ones(n_ages),  ## middle
                                  0.75 * np.ones(n_ages)])  ## high

    ve_gp_laiv = np.array([0.55 * np.ones(n_ages),  ## standard
                           0.55 * np.ones(n_ages),  ## low
                           0.65 * np.ones(n_ages),  ## middle
                           0.80 * np.ones(n_ages)])  ## high

    ve_hosp_laiv = np.copy(ve_gp_laiv)  ## same as ve_gp

    ## Vaccine effectiveness by the chosen vaccine policy and vaccine scenario: initiate with status quo scenario
    ve_inf_main = 0.44 * np.ones(n_ages)
    ve_gp_main = 0.55 * np.ones(n_ages)
    ve_hosp_main = np.copy(ve_gp_main)

    ## Assign an index by policy and age so we can update the ve_[inf/gp/hosp]_main accordingly by vaccine_policy
    my_policy_index = 0  ## status quo
    if vaccine_policy.lower() != 'status_quo':
        if vaccine_scenario.lower() == 'pessimistic':
            my_policy_index = 1
        elif vaccine_scenario.lower() == 'mid':
            my_policy_index = 2
        elif vaccine_scenario.lower() == 'optimistic':
            my_policy_index = 3

    my_age_index = []  ## status quo
    if vaccine_policy.lower() != 'status_quo':
        if vaccine_policy.lower() == 'laiv_5_12':
            my_age_index = [3]  ## 5-11 YO
        elif vaccine_policy.lower() == 'laiv_5_18':
            my_age_index = [3, 4]  ## 5-17 YO

    ## update the main VE arrays to policy program
    for i in my_age_index:
        ve_inf_main[i] = ve_infection_laiv[my_policy_index, i]
        ve_gp_main[i] = ve_gp_laiv[my_policy_index, i]
        ve_hosp_main[i] = ve_hosp_laiv[my_policy_index, i]

    vaccine_coverage = vc_of_vaccine_policy[my_policy_index]

    return [vaccine_coverage, ve_inf_main, ve_gp_main, ve_hosp_main]

def load_VE_and_VC_LAIV2(vaccine_policy='status_quo', vaccine_scenario='mid'):
    ''' for each vaccine policy (status_quo, laiv_2_4, laiv_2_12, laiv_2_18) and
    each vaccine scenario (status_quo, pessimistic, middle, optimistic):
    loads the vaccine coverage and vaccine effectiveness.

    'status_quo' vaccine policy will give status quo VE and VC (regardless of vaccine scenario)
    '''

    n_ages = 10

    if vaccine_scenario not in ['status_quo', 'pessimistic', 'mid', 'optimistic']:
        print('Warning: vaccine_scenario is not status_quo, pessimistic, mid, or optimistic.')
    if vaccine_policy not in ['status_quo', 'laiv_2_4', 'laiv_2_12', 'laiv_2_18']:
        print('Warning: vaccine_policy is not status_quo, laiv_2_4, laiv_2_12, or laiv_2_18.')

    #### Vaccine coverage by vaccine scenario: status quo, pessimistic, mid, optimistic
    ## LAIV 2 to 4
    vc4 = np.zeros((4, n_ages))
    vc4[:, 3] = np.array([0, 0.40, 0.40, 0.40])  ## standard, low, middle, high

    ## LAIV 2 to 12:
    vc12 = np.zeros((4, n_ages))
    vc12[:, 3] = np.array([0, 0.40, 0.40, 0.40])  ## standard, low, middle, high
    vc12[:, 4] = np.array([0, 0.40, 0.60, 0.80])  ## standard, low, middle, high


    ## LAIV 2 to 18:
    vc18 = np.zeros((4, n_ages))
    vc18[:, 3] = np.array([0, 0.40, 0.40, 0.40])  ## standard, low, middle, high
    vc18[:, 4] = np.array([0, 0.40, 0.60, 0.80])  ## 5-11: standard, low, middle, high
    vc18[:, 5] = np.array([0, 0.40, 0.60, 0.80])  ## 12-17: standard, low, middle, high

    vc_of_vaccine_policy = vc4
    if vaccine_policy.lower() == 'laiv_2_12':
        vc_of_vaccine_policy = vc12
    elif vaccine_policy.lower() == 'laiv_2_18':
        vc_of_vaccine_policy = vc18

    vc_of_vaccine_policy[:, 3] = 0

    #### Vaccine effectiveness by vaccine scenario: status quo, pessimistic, mid, optimistic
    ## laiv
    ve_infection_laiv = np.array([0.44 * np.ones(n_ages),  ## standard
                                  0.44 * np.ones(n_ages),  ## low
                                  0.56 * np.ones(n_ages),  ## middle
                                  0.75 * np.ones(n_ages)]) ## high

    ve_gp_laiv = np.array([0.55 * np.ones(n_ages),  ## standard
                           0.55 * np.ones(n_ages),  ## low
                           0.65 * np.ones(n_ages),  ## middle
                           0.80 * np.ones(n_ages)]) ## high

    ve_hosp_laiv = np.copy(ve_gp_laiv)  ## same as ve_gp

    ## Vaccine effectiveness by the chosen vaccine policy and vaccine scenario: initiate with status quo scenario
    ve_inf_main = 0.44 * np.ones(n_ages)
    ve_gp_main = 0.55 * np.ones(n_ages)
    ve_hosp_main = np.copy(ve_gp_main)

    ## Assign an index by policy and age so we can update the ve_[inf/gp/hosp]_main accordingly by vaccine_policy
    my_policy_index = 0  ## status quo
    if vaccine_policy.lower() != 'status_quo':
        if vaccine_scenario.lower() == 'pessimistic':
            my_policy_index = 1
        elif vaccine_scenario.lower() == 'mid':
            my_policy_index = 2
        elif vaccine_scenario.lower() == 'optimistic':
            my_policy_index = 3

    my_age_index = []  ## status quo
    if vaccine_policy.lower() != 'status_quo':
        if vaccine_policy.lower() == 'laiv_2_4':
            my_age_index = [3]
        elif vaccine_policy.lower() == 'laiv_2_12':
            my_age_index = [3, 4]  ## 2-11 YO
        elif vaccine_policy.lower() == 'laiv_2_18':
            my_age_index = [3, 4, 5]  ## 2-17 YO

    ## update the main VE arrays to policy program
    for i in my_age_index:
        ve_inf_main[i] = ve_infection_laiv[my_policy_index, i]
        ve_gp_main[i] = ve_gp_laiv[my_policy_index, i]
        ve_hosp_main[i] = ve_hosp_laiv[my_policy_index, i]

    vaccine_coverage = vc_of_vaccine_policy[my_policy_index]

    return [vaccine_coverage, ve_inf_main, ve_gp_main, ve_hosp_main]



## Default model parameter values

## in run_model, R0 and amplitude will be arguments

n_ages = len(age_names)
n_states = len(state_names)
N = 1_000_000
populations_by_fractions = np.array(load_population_australia()['proportion'])

## Disease natural history parameters (rates per day)
sigma = 1 / 1  ## latent period
gamma = 1 / 2.3  ## infectious period
omega = 0    ## no waning

## Fitted parameter values
## These parameters will be loaded separately by read_parameters_sensitivity_analysis() to make up 40 trajectories
R0 = 1.178539
amplitude = 1.528736e-07
phase_shift = 167.637059
baseline_healthseeking = np.array([0.093087, 0.093087, 0.093087, 0.091192, 0.091192,
                                   0.101058, 0.101058, 0.111099 , 0.111099])
# initial_SW = 0.15

##
p_asymptomatic = 0.19 * np.ones(n_ages)
p_hospitalised = load_hospitalisation_rates_aihw() / 100_000
p_deaths = load_death_rates() / 100_000
reduced_sus = load_reduced_susceptibility()

## Simulation period
total_weeks = 52
n_days = total_weeks * 7

## Days 0 to 364, inclusive
tspan = np.arange(n_days + 1)

