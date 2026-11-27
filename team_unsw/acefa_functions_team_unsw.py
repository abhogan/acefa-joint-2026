## functions for ACEFA scenario modelling
import pandas as pd
import numpy as np
from scipy.integrate import odeint
# from scipy.stats import poisson
# from scipy import optimize
# from mizani.breaks import date_breaks
# from mizani.formatters import date_format


def load_contact_matrix(my_population=np.zeros((9,9)), n_ages = 9):
    ''' my_population is the population percentages by age group.
    contact matrix from POLYMOD study '''

    C = np.ones((n_ages, n_ages))

    if n_ages == 9:
        C = pd.read_csv('data/ACEFA_UK_POLYMOD_contact_matrix.csv', header=None).to_numpy()
    elif n_ages == 10:
        C = pd.read_csv('data/ACEFA_UK_POLYMOD_contact_matrix_laiv_2_4.csv', header=None).to_numpy()

    return C

def findBeta(R0, C, gamma, mu, totalPop):
    # compute the eigenvalues of -F*V^(-1)
    [n1, n2] = np.shape(C)

    # create F (transmission matrix) and V (transition matrix)
    F = np.zeros((n1, n1))
    V = np.diag(-(gamma + mu) * np.ones(n1))
    N = np.sum(totalPop)

    for jvals in range(n1):
        F[jvals, :] = (C[jvals, :] / N) * totalPop[jvals]

    FV = np.dot(-F, np.linalg.inv(V))

    myEig = np.linalg.eig(FV)
    largestEig = np.max(myEig[0])

    assert largestEig.imag == 0.0, 'largest eigenvalue is not real'
    beta = R0 / largestEig.real
    return beta

def model_equations_seir(initial_population, t, parameters):
    '''age-structured ACEFA model for a single population with seasonality
    SEIR model for unvaccinated, vaccinated, and pre-existing immunity "lines"
    '''

    ## Ensure my population is non-negative
    initial_population_pos = np.maximum(initial_population, 0)

    [amplitude, beta, contact_matrix, gamma, omega, phase_shift, n_ages, n_states, sigma, VE_SUS, reduced_sus] = parameters

    population_states = np.reshape(initial_population_pos, (n_states + 3, n_ages))

    SU, E1U, E2U, IU, RU = population_states[0:5]  ## unvaccinated
    SV, E1V, E2V, IV, RV = population_states[5:10]  ## vaccinated
    SW, E1W, E2W, IW, RW = population_states[10:15]  ## preexisting immunity

    FOI = (1 - amplitude * np.cos(2 * np.pi * (t + phase_shift) / 371)) * np.dot(contact_matrix, beta * (IU + IV + IW))

    dSU = - np.multiply(FOI, SU)
    dE1U = np.multiply(FOI, SU) - (2 * sigma) * E1U
    dE2U = (2 * sigma) * E1U - (2 * sigma) * E2U
    dIU = (2 * sigma) * E2U - gamma * IU
    dRU = gamma * IU - omega * RU
    #
    dSV = - np.multiply( (1 - VE_SUS) * FOI, SV) - omega * SV
    dE1V = np.multiply( (1 - VE_SUS) * FOI, SV) - (2 * sigma) * E1V
    dE2V = (2 * sigma) * E1V - (2 * sigma) * E2V
    dIV = (2 * sigma) * E2V - gamma * IV
    dRV = gamma * IV - omega * RV
    #
    dSW = omega * SV + omega * (RU + RV + RW) - np.multiply((1 - reduced_sus) * FOI, SW)
    dE1W = np.multiply((1 - reduced_sus) * FOI, SW) - (2 * sigma) * E1W
    dE2W = (2 * sigma) * E1W - (2 * sigma) * E2W
    dIW = (2 * sigma) * E2W - gamma * IW
    dRW = gamma * IW - omega * RW
    #
    ## record recoveries for weekly recordables
    count_IU = gamma * IU
    count_IV = gamma * IV
    count_IW = gamma * IW
    #
    dydt = np.array([dSU, dE1U, dE2U, dIU, dRU,
                     dSV, dE1V, dE2V, dIV, dRV,
                     dSW, dE1W, dE2W, dIW, dRW,
                     count_IU, count_IV, count_IW]).reshape(( (n_states + 3) * n_ages ))

    return dydt

def get_weekly_vaccination_rates():
    ''' reads the weekly vaccination rates data for 2021, 2022, 2023, and 2024 and calculates the average weekly
     speed of vaccinating those who decide to get vaccinated over 24 weeks (Mar 7 to Aug 31).
     OUTPUT is an array (length 24) of cumulative %age completion of the overall vaccination'''

    cumulative_weekly_vacc_progress_iiv = np.array([0.00266946, 0.00334557, 0.00737364, 0.03162889, 0.07259051,
           0.1482384, 0.24145878, 0.36451686, 0.47168377, 0.56855491,
           0.65137032, 0.75339126, 0.81009063, 0.84931351, 0.88319599,
           0.91812563, 0.93545698, 0.95122565, 0.96457128, 0.97785923,
           0.9846138, 0.99014375, 0.99475859, 1.])

    my_dates = np.array(['Mar 07', 'Mar 14', 'Mar 21', 'Mar 31', 'Apr 07', 'Apr 14',
       'Apr 21', 'Apr 30', 'May 7', 'May 14', 'May 21', 'May 31',
       'Jun 07', 'Jun 14', 'Jun 21', 'Jun 30', 'Jul 07', 'Jul 14',
       'Jul 21', 'Jul 31', 'Aug 07', 'Aug 14', 'Aug 21', 'Aug 31'],
                        dtype=object)

    return cumulative_weekly_vacc_progress_iiv, my_dates

def get_weekly_vaccination_rates_LAIV():
    ''' LAIV program over 6 weeks (Mar 1 to Apr 11, say) in school term 1
     OUTPUT is an array (length 24) of cumulative %age completion of the overall vaccination'''

    x = np.array([0.16666667, 0.33333333, 0.5       , 0.66666667, 0.83333333,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        ])

    my_dates = np.array(['Mar 07', 'Mar 14', 'Mar 21', 'Mar 31', 'Apr 07', 'Apr 14',
       'Apr 21', 'Apr 30', 'May 7', 'May 14', 'May 21', 'May 31',
       'Jun 07', 'Jun 14', 'Jun 21', 'Jun 30', 'Jul 07', 'Jul 14',
       'Jul 21', 'Jul 31', 'Aug 07', 'Aug 14', 'Aug 21', 'Aug 31'],
      dtype=object)

    return x, my_dates

def get_weekly_vaccination_rates_preschool_12weeks():
    ''' reads the weekly vaccination rates data for 2021, 2022, 2023, and 2024 and calculates the average weekly
     speed of vaccinating those who decide to get vaccinated over 24 weeks (Mar 7 to Aug 31).
     OUTPUT is an array (length 24) of cumulative %age completion of the overall vaccination'''

    cumulative_weekly_vacc_progress_preschool = np.array([0.00334557, 0.03162889, 0.1482384 , 0.36451686, 0.56855491,
       0.75339126, 0.84931351, 0.91812563, 0.95122565, 0.97785923,
       0.99014375, 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        , 1.        ,
       1.        , 1.        , 1.        , 1.        ])

    my_dates = np.array(['Mar 07', 'Mar 14', 'Mar 21', 'Mar 31', 'Apr 07', 'Apr 14',
       'Apr 21', 'Apr 30', 'May 7', 'May 14', 'May 21', 'May 31',
       'Jun 07', 'Jun 14', 'Jun 21', 'Jun 30', 'Jul 07', 'Jul 14',
       'Jul 21', 'Jul 31', 'Aug 07', 'Aug 14', 'Aug 21', 'Aug 31'],
      dtype=object)

    return cumulative_weekly_vacc_progress_preschool, my_dates



## Calculated outcomes

def get_hospitalisations_per_infection(y, populations_by_fractions, hosp_per_population, p_asymptomatic):
    '''
    Calculate age-specific hospitalisations per infection.
    '''

    # Daily new infections by vaccination status
    i_unvaccinated = np.diff(y[:, 15, :], axis=0)
    i_vaccinated = np.diff(y[:, 16, :], axis=0)
    i_waning = np.diff(y[:, 17, :], axis=0)

    # Total infections and age-specific attack rates
    total_infections = (i_unvaccinated + i_vaccinated + i_waning).sum(axis=0)
    attack_rates = total_infections / populations_by_fractions

    # Hospitalisations per symptomatic infection
    hosp_per_infection = hosp_per_population / (attack_rates * (1 - p_asymptomatic))

    return hosp_per_infection

def get_deaths_per_infection(y, populations_by_fractions, death_per_population, p_asymptomatic):
    '''
    Calculate age-specific deaths per infection.
    '''

    # Daily new infections by vaccination status
    i_unvaccinated = np.diff(y[:, 15, :], axis=0)
    i_vaccinated = np.diff(y[:, 16, :], axis=0)
    i_waning = np.diff(y[:, 17, :], axis=0)

    # Total infections and age-specific attack rates
    total_infections = (i_unvaccinated + i_vaccinated + i_waning).sum(axis=0)
    attack_rates = total_infections / populations_by_fractions

    # Deaths per symptomatic infection
    death_per_infection = death_per_population / (attack_rates * (1 - p_asymptomatic))

    return death_per_infection

def calculate_weekly_outcomes(y, parameters):
    '''takes a daily time series (of length 7 * total_weeks + 1) and returns weekly infections, weekly (observed) cases, and
    weekly hospitalizations
    y.shape = (365, 18, 9) or (days, n_states + 3, n_ages)'''

    [total_weeks, N, VE_INF0, VE_GP0, VE_HOSP0, p_asymptomatic,
     p_healthseeking, hosp_per_infection, death_per_infection] = parameters

    y_IU, y_IV, y_IW = y[:, 15, :], y[:, 16, :], y[:, 17, :]
    i_unvacc = (y_IU[1:] - y_IU[:-1]) * N
    i_vacc = (y_IV[1:] - y_IV[:-1]) * N
    i_waning = (y_IW[1:] - y_IW[:-1]) * N
    #

    ## Recalculate VE conditional on infection
    VE_HOSP = 1 - np.divide(1 - VE_HOSP0, 1 - VE_INF0)
    VE_GP = 1 - np.divide(1 - VE_GP0, 1 - VE_INF0)

    my_infections = np.sum(i_unvacc + i_vacc + i_waning, -1)

    my_cases = np.dot((1 - p_asymptomatic) * i_unvacc, p_healthseeking) + \
               np.dot((1 - VE_GP) * (1 - p_asymptomatic) * i_vacc, p_healthseeking) + \
               np.dot((1 - p_asymptomatic) * i_waning, p_healthseeking)

    my_hospitalizations = np.dot((1 - p_asymptomatic) * i_unvacc, hosp_per_infection) + \
                          np.dot((1 - VE_HOSP) * (1 - p_asymptomatic) * i_vacc, hosp_per_infection) + \
                          np.dot((1 - p_asymptomatic) * i_waning, hosp_per_infection)

    my_deaths = np.dot((1 - p_asymptomatic) * i_unvacc, death_per_infection) + \
                np.dot((1 - VE_HOSP) * (1 - p_asymptomatic) * i_vacc, death_per_infection) + \
                np.dot((1 - p_asymptomatic) * i_waning, death_per_infection)

    weekly_infections = my_infections.reshape((-1, 7)).sum(1)
    weekly_cases = my_cases.reshape((-1, 7)).sum(1)
    weekly_hospitalizations = my_hospitalizations.reshape((-1, 7)).sum(1)
    weekly_deaths = my_deaths.reshape((-1, 7)).sum(1)

    return [weekly_infections, weekly_cases, weekly_hospitalizations, weekly_deaths]

def calculate_total_outcomes_by_age(y, parameters):
    '''  '''
    [total_weeks, N, VE_INF0, VE_GP0, VE_HOSP0,
     p_asymptomatic, p_healthseeking, hosp_per_infection, death_per_infection] = parameters

    y_IU, y_IV, y_IW = y[:, 15, :], y[:, 16, :], y[:, 17, :]
    i_unvacc = (y_IU[1:] - y_IU[:-1]) * N
    i_vacc = (y_IV[1:] - y_IV[:-1]) * N
    i_waning = (y_IW[1:] - y_IW[:-1]) * N

    ## Recalculate VE conditional on infection
    VE_HOSP = 1 - np.divide(1 - VE_HOSP0, 1 - VE_INF0)
    VE_GP = 1 - np.divide(1 - VE_GP0, 1 - VE_INF0)

    my_infections = i_unvacc + i_vacc + i_waning

    my_cases = np.multiply((1 - p_asymptomatic) * i_unvacc, p_healthseeking) + \
               np.multiply((1 - VE_GP) * (1 - p_asymptomatic) * i_vacc, p_healthseeking) + \
               np.multiply((1 - p_asymptomatic) * i_waning, p_healthseeking)

    my_hospitalizations = np.multiply((1 - p_asymptomatic) * i_unvacc, hosp_per_infection) + \
                          np.multiply((1 - VE_HOSP) * (1 - p_asymptomatic) * i_vacc, hosp_per_infection) + \
                          np.multiply((1 - p_asymptomatic) * i_waning, hosp_per_infection)

    my_deaths = np.multiply((1 - p_asymptomatic) * i_unvacc, death_per_infection) + \
                np.multiply((1 - VE_HOSP) * (1 - p_asymptomatic) * i_vacc, death_per_infection) + \
                np.multiply((1 - p_asymptomatic) * i_waning, death_per_infection)

    total_infections = my_infections.sum(0)
    total_cases = my_cases.sum(0)
    total_hospitalizations = my_hospitalizations.sum(0)
    total_deaths = my_deaths.sum(0)

    return [total_infections, total_cases, total_hospitalizations, total_deaths]

def calculate_weekly_outcomes_by_age(y, parameters):
    [total_weeks, N, VE_INF0, VE_GP0, VE_HOSP0, p_asymptomatic,
     p_healthseeking, hosp_per_infection, death_per_infection] = parameters

    y_IU, y_IV, y_IW = y[:, 15, :], y[:, 16, :], y[:, 17, :]
    i_unvacc = (y_IU[1:] - y_IU[:-1]) * N
    i_vacc = (y_IV[1:] - y_IV[:-1]) * N
    i_waning = (y_IW[1:] - y_IW[:-1]) * N
    my_infections = i_unvacc + i_vacc + i_waning
    #
    ## Recalculate VE conditional on infection
    VE_HOSP = 1 - np.divide(1 - VE_HOSP0, 1 - VE_INF0)
    VE_GP = 1 - np.divide(1 - VE_GP0, 1 - VE_INF0)

    my_cases = np.multiply((1 - p_asymptomatic) * i_unvacc, p_healthseeking) + \
               np.multiply((1 - VE_GP) * (1 - p_asymptomatic) * i_vacc, p_healthseeking) + \
               np.multiply((1 - p_asymptomatic) * i_waning, p_healthseeking)

    my_hospitalizations = np.multiply((1 - p_asymptomatic) * i_unvacc, hosp_per_infection) + \
                          np.multiply((1 - VE_HOSP) * (1 - p_asymptomatic) * i_vacc, hosp_per_infection) + \
                          np.multiply((1 - p_asymptomatic) * i_waning, hosp_per_infection)

    my_deaths = np.multiply((1 - p_asymptomatic) * i_unvacc, death_per_infection) + \
                np.multiply((1 - VE_HOSP) * (1 - p_asymptomatic) * i_vacc, death_per_infection) + \
                np.multiply((1 - p_asymptomatic) * i_waning, death_per_infection)

    if my_infections.shape[0] % 7 != 0:
        print("Warning: The number of rows is not a multiple of 7. Truncating the array.")
        y = my_infections[:-(my_infections.shape[0] % 7)]

    weekly_infections_by_age = my_infections.reshape(my_infections.shape[0] // 7, 7, my_infections.shape[1]).sum(1)

    if my_cases.shape[0] % 7 != 0:
        print("Warning: The number of rows is not a multiple of 7. Truncating the array.")
        y = my_cases[:-(my_cases.shape[0] % 7)]

    weekly_cases_by_age = my_cases.reshape(my_cases.shape[0] // 7, 7, my_cases.shape[1]).sum(1)

    if my_hospitalizations.shape[0] % 7 != 0:
        print("Warning: The number of rows is not a multiple of 7. Truncating the array.")
        y = my_hospitalizations[:-(my_hospitalizations.shape[0] % 7)]

    weekly_hospitalizations_by_age = my_hospitalizations.reshape(my_hospitalizations.shape[0] // 7, 7, my_hospitalizations.shape[1]).sum(1)

    if my_deaths.shape[0] % 7 != 0:
        print("Warning: The number of rows is not a multiple of 7. Truncating the array.")
        y = my_deaths[:-(my_deaths.shape[0] % 7)]

    weekly_deaths_by_age = my_deaths.reshape(my_deaths.shape[0] // 7, 7, my_deaths.shape[1]).sum(1)

    return [weekly_infections_by_age, weekly_cases_by_age, weekly_hospitalizations_by_age, weekly_deaths_by_age]

def calculate_daily_outcomes_by_age(y, parameters):
    [total_weeks, N, VE_INF0, VE_GP0, VE_HOSP0, p_asymptomatic,
     p_healthseeking, hosp_per_infection, death_per_infection] = parameters

    # y = out_main.reshape((-1, n_states + 3, n_ages))

    y_IU, y_IV, y_IW = y[:, 15, :], y[:, 16, :], y[:, 17, :]
    i_unvacc = (y_IU[1:] - y_IU[:-1]) * N
    i_vacc = (y_IV[1:] - y_IV[:-1]) * N
    i_waning = (y_IW[1:] - y_IW[:-1]) * N
    my_infections = i_unvacc + i_vacc + i_waning

    ## Recalculate VE conditional on infection
    VE_HOSP = 1 - np.divide(1 - VE_HOSP0, 1 - VE_INF0)
    VE_GP = 1 - np.divide(1 - VE_GP0, 1 - VE_INF0)

    my_cases = np.multiply((1 - p_asymptomatic) * i_unvacc, p_healthseeking) + \
               np.multiply((1 - VE_GP) * (1 - p_asymptomatic) * i_vacc, p_healthseeking) + \
               np.multiply((1 - p_asymptomatic) * i_waning, p_healthseeking)

    my_hospitalizations = np.multiply((1 - p_asymptomatic) * i_unvacc, hosp_per_infection) + \
                          np.multiply((1 - VE_HOSP) * (1 - p_asymptomatic) * i_vacc, hosp_per_infection) + \
                          np.multiply((1 - p_asymptomatic) * i_waning, hosp_per_infection)

    my_deaths = np.multiply((1 - p_asymptomatic) * i_unvacc, death_per_infection) + \
                np.multiply((1 - VE_HOSP) * (1 - p_asymptomatic) * i_vacc, death_per_infection) + \
                np.multiply((1 - p_asymptomatic) * i_waning, death_per_infection)

    return [my_infections, my_cases, my_hospitalizations, my_deaths]

