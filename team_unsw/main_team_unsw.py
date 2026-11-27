## ACEFA Scenario Modelling: Team UNSW

import numpy as np
import pandas as pd
from scipy.integrate import odeint
from pathlib import Path
import matplotlib
matplotlib.use('TkAgg')

import matplotlib.pyplot as plt
# import seaborn as sns
# import csv
# import pickle

from acefa_functions_team_unsw import *
from default_params_team_unsw import *

POLICIES = ('laiv_5_12', 'laiv_5_18')
SCENARIOS = ('status_quo', 'pessimistic', 'mid', 'optimistic')
OUTCOMES = ('infections', 'cases', 'hospitalisations', 'deaths')
REPORTING_AGES = ('<1', '1-4', '5-11', '12-17', '18-64', '65-79', '80+')
TARGETS = ('infection_incidence', 'disease_incidence', 'admission_incidence',
           'death_incidence', 'IIV_doses', 'LAIV_doses')


def calculate_outcomes(solution, parameters):
    '''Calculate weekly, weekly-by-age, total-by-age and daily-by-age outcomes.'''

    return {'weekly': calculate_weekly_outcomes(y=solution, parameters=parameters),
            'weekly_age': calculate_weekly_outcomes_by_age(y=solution, parameters=parameters),
            'total_age': calculate_total_outcomes_by_age(y=solution, parameters=parameters),
            'daily_age': calculate_daily_outcomes_by_age(y=solution, parameters=parameters)}

def read_parameters_sensitivity_analysis(p_healthseeking):
    ''' reads a file with 40 sets of parameters for sensitivity analysis'''

    df = pd.read_csv('data/optimization_results_p_hs_scale_wo_a_ps_365.csv')

    n_sims, n_ages = df.shape

    ## my params
    my_inits = np.zeros((n_sims, (n_states + 3) * n_ages))
    xmy_p_healthseekings = np.zeros((n_sims, n_ages))
    my_r0s = df["R₀"]
    my_amplitudes = df["amplitude"]
    my_phaseshifts = df["phase-shift"]
    my_sws = df["SW_vals"]
    my_is = df["seed"]

    for sval in range(n_sims):
        _, my_sw, my_i, my_r0, my_tau_child, my_tau_adult, my_amp, my_phaseshift, _ = df.iloc[sval]

        ## initial condition
        new_init = np.zeros((n_states + 3, n_ages))
        new_init[SU, :] = np.ones(n_ages) * populations_by_fractions - my_sw * populations_by_fractions
        new_init[IU, 5] = my_i
        new_init[SU, 5] -= my_i
        new_init[CaseIU, :] = new_init[IU, :]  ## number of IU's
        new_init[SW, :] = my_sw * populations_by_fractions

        my_inits[sval] = new_init.reshape((n_states + 3) * n_ages)

        ## p_healthseeking
        xmy_p_healthseeking = p_healthseeking[5] * np.ones(n_ages)
        xmy_p_healthseeking[:5] = p_healthseeking[:5] * my_tau_child
        xmy_p_healthseeking[7:9] = p_healthseeking[7:9] * my_tau_adult
        xmy_p_healthseekings[sval] = xmy_p_healthseeking

    return [my_inits, xmy_p_healthseekings, my_r0s, my_amplitudes, my_phaseshifts, my_sws, my_is]

def read_parameters_sensitivity_analysis_preschool(p_healthseeking):
    ''' reads a file with 40 sets of parameters for sensitivity analysis'''

    df = pd.read_csv('data/optimization_results_p_hs_scale_wo_a_ps_365.csv')

    n_sims, n_ages = df.shape
    n_ages = 10

    ## my params
    my_inits = np.zeros((n_sims, (n_states + 3) * n_ages))
    xmy_p_healthseekings = np.zeros((n_sims, n_ages))
    my_r0s = df["R₀"]
    my_amplitudes = df["amplitude"]
    my_phaseshifts = df["phase-shift"]
    my_sws = df["SW_vals"]
    my_is = df["seed"]

    for sval in range(n_sims):
        _, my_sw, my_i, my_r0, my_tau_child, my_tau_adult, my_amp, my_phaseshift, _ = df.iloc[sval]

        ## initial condition
        new_init = np.zeros((n_states + 3, n_ages))
        new_init[SU, :] = np.ones(n_ages) * populations_by_fractions - my_sw * populations_by_fractions
        new_init[IU, 6] = my_i
        new_init[SU, 6] -= my_i
        new_init[CaseIU, :] = new_init[IU, :]  ## number of IU's
        new_init[SW, :] = my_sw * populations_by_fractions

        my_inits[sval] = new_init.reshape((n_states + 3) * n_ages)

        ## p_healthseeking
        xmy_p_healthseeking = p_healthseeking[6] * np.ones(n_ages)
        xmy_p_healthseeking[:6] = p_healthseeking[:6] * my_tau_child
        xmy_p_healthseeking[8:10] = p_healthseeking[8:10] * my_tau_adult
        xmy_p_healthseekings[sval] = xmy_p_healthseeking


    return [my_inits, xmy_p_healthseekings, my_r0s, my_amplitudes, my_phaseshifts, my_sws, my_is]

def run_model_equations_weekly_vaccination_3_laiv_multirun(initial_population, amplitude = 0.3, R0 = 1.5,
       vaccine_uptake = 'pessimistic', vaccine_policy='status_quo', contact_matrix = np.ones((n_ages, n_ages))):
    '''
    Run 40 trajectories from model calibration for the ACEFA Scenario Modelling exercise for
    LAIV 5-12 and LAIV 5-18 (9 age groups).

    LAIV and IIV rollouts are run over 6 and 24 weeks respectively.

    vaccine_uptake :: 'pessimistic' OR 'mid' OR 'optimistic'
    vaccine_policy :: 'status_quo' OR 'laiv_5_12' OR 'laiv_5_18'

    Returns a daily time series of (n_states + 3) and n_ages
    '''

    ## transmission and vaccine params
    beta = findBeta(R0, contact_matrix, gamma, 0, populations_by_fractions)

    vc_laiv, VE_SUS, VE_GP, VE_HOSP = load_VE_and_VC_LAIV(vaccine_policy=vaccine_policy, vaccine_scenario=vaccine_uptake)
    vc_iiv, VE_SUS, VE_GP, VE_HOSP = load_VE_and_VC_IIV(vaccine_policy=vaccine_policy, vaccine_scenario=vaccine_uptake)

    parameters_ode = [amplitude, beta, contact_matrix, gamma, omega, phase_shift, n_ages, n_states, sigma, VE_SUS, reduced_sus]

    out_main = np.copy(initial_population).reshape((n_states + 3) * n_ages)  ## to be vstacked

    weeks_no_vacc = 12
    vacc_weeks = 24

    out, info = odeint(model_equations_seir, initial_population.reshape((n_states + 3) * n_ages), tspan[0:(weeks_no_vacc*7)],
                       args=(parameters_ode,), full_output=True)

    out_main = np.vstack((out_main, out))

    old_init = out[-1].reshape((n_states + 3, n_ages))

    cum_weekly_vacc_progress, _ = get_weekly_vaccination_rates()
    cum_weekly_vacc_progress_laiv, _ = get_weekly_vaccination_rates_LAIV()

    cum_weekly_vaccination_schedule_iiv = np.zeros((n_ages, vacc_weeks + 1))
    new_weekly_vaccination_schedule_iiv = np.zeros((n_ages, vacc_weeks))

    cum_weekly_vaccination_schedule_laiv = np.zeros((n_ages, vacc_weeks + 1))
    new_weekly_vaccination_schedule_laiv = np.zeros((n_ages, vacc_weeks))

    for age_index in range(n_ages):
        cum_weekly_vaccination_schedule_iiv[age_index, 1:] = cum_weekly_vacc_progress * vc_iiv[age_index]
        new_weekly_vaccination_schedule_iiv[age_index] = cum_weekly_vaccination_schedule_iiv[age_index][1:] - \
                                             cum_weekly_vaccination_schedule_iiv[age_index][:-1]
        cum_weekly_vaccination_schedule_laiv[age_index, 1:] = cum_weekly_vacc_progress_laiv * vc_laiv[age_index]
        new_weekly_vaccination_schedule_laiv[age_index] = cum_weekly_vaccination_schedule_laiv[age_index][1:] - \
                                             cum_weekly_vaccination_schedule_laiv[age_index][:-1]


    # set initial condition with vaccination
    my_t0 = int(tspan[0:(weeks_no_vacc*7)][-1] + 1)
    total_doses_frac = np.zeros(n_ages)
    total_iiv_doses_frac_daily = np.zeros((52 * 7, n_ages))
    total_laiv_doses_frac_daily = np.zeros((52 * 7, n_ages))

    for vacc_week in range(vacc_weeks):
        ## do IIV campaign
        vc_iiv = new_weekly_vaccination_schedule_iiv[:, vacc_week]
        init_new = np.copy(old_init.reshape((n_states + 3, n_ages)))  ## needs to be 18,9

        ## Eligible population
        unvaccinated_fraction_of_SR = (old_init[(SU, SW, RU, RW), :] * (vc_iiv > 0)).sum() / (populations_by_fractions * (vc_iiv > 0)).sum()

        n_doses_actual = (populations_by_fractions * N) * vc_iiv  ## length n_ages
        vc_weekly_actual = n_doses_actual / (unvaccinated_fraction_of_SR * (populations_by_fractions * N))

        for old_state, new_state in zip([SU, SW, RU, RW], [SV, SV, RV, RV]):

            frac_to_vaccinate = vc_weekly_actual * old_init[old_state]
            init_new[old_state] = old_init[old_state] - frac_to_vaccinate
            init_new[new_state] += frac_to_vaccinate
            total_doses_frac += frac_to_vaccinate

            my_dose_week = (vacc_week + 1) * 7 + weeks_no_vacc*7 - 1
            total_iiv_doses_frac_daily[my_dose_week, :] = n_doses_actual

        ## do LAIV campaign
        vc_laiv = new_weekly_vaccination_schedule_laiv[:, vacc_week]
        if vc_laiv.sum() > 0:
            ## Eligible population
            unvaccinated_fraction_of_SR = (old_init[(SU, SW, RU, RW), :] * (vc_laiv > 0)).sum() / (populations_by_fractions * (vc_laiv > 0)).sum()
            n_doses_actual = (populations_by_fractions * N) * vc_laiv  ## length n_ages
            vc_weekly_actual = n_doses_actual / (unvaccinated_fraction_of_SR * (populations_by_fractions * N))

            for old_state, new_state in zip([SU, SW, RU, RW], [SV, SV, RV, RV]):

                frac_to_vaccinate = vc_weekly_actual * init_new[old_state]
                init_new[old_state] = init_new[old_state] - frac_to_vaccinate
                init_new[new_state] += frac_to_vaccinate
                total_doses_frac += frac_to_vaccinate

                my_dose_week = (vacc_week + 1) * 7 + weeks_no_vacc * 7 - 1
                total_laiv_doses_frac_daily[my_dose_week, :] = n_doses_actual

        out, info = odeint(model_equations_seir, init_new.reshape((n_states + 3) * n_ages), tspan[my_t0:my_t0 + 8],
                           args=(parameters_ode,), full_output=True)

        out_main = np.vstack((out_main, out[1:]))

        ## update initial condition and time span
        old_init = out[-1].reshape((n_states + 3, n_ages))
        my_t0 += 7

    total_doses = N * total_doses_frac

    out, info = odeint(model_equations_seir, old_init.reshape((n_states + 3) * n_ages), tspan[my_t0:],
                       args=(parameters_ode,), full_output=True)

    out_main = np.vstack((out_main, out[1:]))

    return out_main, total_doses, total_iiv_doses_frac_daily, total_laiv_doses_frac_daily

def run_model_equations_weekly_vaccination_3_laiv_preschool12_multirun(initial_population, amplitude = 0.3, R0 = 1.5, vaccine_uptake = 'mid', vaccine_policy='laiv_2_4',
                                             contact_matrix = np.ones((n_ages, n_ages))):
    ''' Run 40 trajectories from model calibration for the ACEFA Scenario Modelling exercise for
    pre-school related LAIV programs: LAIV 2-5, LAIV 2-12 and LAIV 2-18 (10 age groups).

    The rollouts are run over:
    - IIV: 24 weeks
    - LAIV 5-12 and 5-18: 6 weeks
    - LAIV 2-5: 12 weeks at twice as fast as IIV

    vaccine_uptake :: 'pessimistic' OR 'mid' OR 'optimistic'
    vaccine_policy :: 'status_quo' OR 'laiv_2_4' OR 'laiv_2_12' OR 'laiv_2_18'

    Returns a daily time series of (n_states + 3) and n_ages
    '''

    ## transmission and vaccine params
    beta = findBeta(R0, contact_matrix, gamma, 0, populations_by_fractions)

    vc_iiv, VE_SUS, VE_GP, VE_HOSP = load_VE_and_VC_IIV2(vaccine_policy=vaccine_policy, vaccine_scenario=vaccine_uptake)
    vc_laiv, VE_SUS, VE_GP, VE_HOSP = load_VE_and_VC_LAIV2(vaccine_policy=vaccine_policy, vaccine_scenario=vaccine_uptake)

    ## create a vc_laiv only for preschool for their 12-week rollout
    vc_laiv_preschool = np.zeros(n_ages)
    if vaccine_policy.lower() != 'status_quo':
        vc_laiv_preschool[3] = vc_iiv[3]
        vc_iiv[3] = 0 ## so they don't get the IIV

    parameters_ode = [amplitude, beta, contact_matrix, gamma, omega, phase_shift, n_ages,
                      n_states, sigma, VE_SUS, reduced_sus]

    out_main = np.copy(initial_population).reshape((n_states + 3) * n_ages)  ## to be vstacked

    ## Simulation settings
    weeks_no_vacc = 12
    vacc_weeks = 24

    out = odeint(model_equations_seir, initial_population.reshape((n_states + 3) * n_ages),
                 tspan[0:(weeks_no_vacc*7)], args=(parameters_ode,))

    out_main = np.vstack((out_main, out))
    old_init = out[-1].reshape((n_states + 3, n_ages))

    ## Weekly vaccination schedule
    cum_weekly_vacc_progress, _ = get_weekly_vaccination_rates()
    cum_weekly_vacc_progress_laiv, _ = get_weekly_vaccination_rates_LAIV()
    cum_weekly_vacc_progress_laiv_preschool, _ = get_weekly_vaccination_rates_preschool_12weeks()

    cum_weekly_vaccination_schedule_iiv = np.zeros((n_ages, vacc_weeks + 1))
    new_weekly_vaccination_schedule_iiv = np.zeros((n_ages, vacc_weeks))

    cum_weekly_vaccination_schedule_laiv = np.zeros((n_ages, vacc_weeks + 1))
    new_weekly_vaccination_schedule_laiv = np.zeros((n_ages, vacc_weeks))

    cum_weekly_vaccination_schedule_laiv_preschool = np.zeros((n_ages, vacc_weeks + 1))
    new_weekly_vaccination_schedule_laiv_preschool = np.zeros((n_ages, vacc_weeks))

    for age_index in range(n_ages):
        cum_weekly_vaccination_schedule_iiv[age_index, 1:] = cum_weekly_vacc_progress * vc_iiv[age_index]
        new_weekly_vaccination_schedule_iiv[age_index] = cum_weekly_vaccination_schedule_iiv[age_index][1:] - \
                                             cum_weekly_vaccination_schedule_iiv[age_index][:-1]
        cum_weekly_vaccination_schedule_laiv[age_index, 1:] = cum_weekly_vacc_progress_laiv * vc_laiv[age_index]
        new_weekly_vaccination_schedule_laiv[age_index] = cum_weekly_vaccination_schedule_laiv[age_index][1:] - \
                                             cum_weekly_vaccination_schedule_laiv[age_index][:-1]
        cum_weekly_vaccination_schedule_laiv_preschool[age_index, 1:] = cum_weekly_vacc_progress_laiv_preschool * vc_laiv_preschool[age_index]
        new_weekly_vaccination_schedule_laiv_preschool[age_index] = cum_weekly_vaccination_schedule_laiv_preschool[age_index][1:] - \
                                                  cum_weekly_vaccination_schedule_laiv_preschool[age_index][:-1]

    # set initial condition with vaccination
    my_t0 = int(tspan[0:(weeks_no_vacc*7)][-1] + 1)
    total_doses_frac = np.zeros(n_ages)
    total_iiv_doses_frac_daily = np.zeros((52 * 7, n_ages))
    total_laiv_doses_frac_daily = np.zeros((52 * 7, n_ages))

    for vacc_week in range(vacc_weeks):
        ## do IIV campaign
        vc_iiv = new_weekly_vaccination_schedule_iiv[:, vacc_week]
        init_new = np.copy(old_init.reshape((n_states + 3, n_ages)))
        for old_state, new_state in zip([SU, SW, RU, RW], [SV, SV, RV, RV]):

            ## Eligible fraction
            unvaccinated_fraction_of_SR = (old_init[(SU, SW, RU, RW), :] * (vc_iiv > 0)).sum() / (populations_by_fractions * (vc_iiv > 0)).sum()

            n_doses_actual = (populations_by_fractions * N) * vc_iiv  ## length n_ages (not taking into account those who are infected)
            vc_weekly_actual = n_doses_actual / (unvaccinated_fraction_of_SR * (populations_by_fractions * N))  ## takes into account those who are actually eligible to get vax'd

            frac_to_vaccinate = vc_weekly_actual * old_init[old_state]

            init_new[old_state] = old_init[old_state] - frac_to_vaccinate
            init_new[new_state] += frac_to_vaccinate
            total_doses_frac += frac_to_vaccinate

            ## track doses by week and by age
            my_dose_week = (vacc_week + 1) * 7 + weeks_no_vacc*7 - 1
            total_iiv_doses_frac_daily[my_dose_week, :] = n_doses_actual

        ## do LAIV campaign for 2-4
        vc_laiv2 = new_weekly_vaccination_schedule_laiv_preschool[:, vacc_week]
        if vc_laiv2.sum() > 0:
            for old_state, new_state in zip([SU, SW, RU, RW], [SV, SV, RV, RV]):

                ## Eligible fraction
                unvaccinated_fraction_of_SR = (old_init[(SU, SW, RU, RW), :] * (vc_laiv2 > 0)).sum() / (populations_by_fractions * (vc_laiv2 > 0)).sum()

                n_doses_actual = (populations_by_fractions * N) * vc_laiv2  ## length n_ages
                vc_weekly_actual = n_doses_actual / (unvaccinated_fraction_of_SR * (populations_by_fractions * N))

                frac_to_vaccinate = vc_weekly_actual * init_new[old_state]

                init_new[old_state] = init_new[old_state] - frac_to_vaccinate
                init_new[new_state] += frac_to_vaccinate
                total_doses_frac += frac_to_vaccinate

                ## track doses by week and by age
                my_dose_week = (vacc_week + 1) * 7 + weeks_no_vacc * 7 - 1
                total_laiv_doses_frac_daily[my_dose_week, :] = n_doses_actual

        ## do LAIV campaign
        vc_laiv = new_weekly_vaccination_schedule_laiv[:, vacc_week]
        if vc_laiv.sum() > 0:
            for old_state, new_state in zip([SU, SW, RU, RW], [SV, SV, RV, RV]):

                ## Eligible fraction
                unvaccinated_fraction_of_SR = (old_init[(SU, SW, RU, RW), :] * (vc_laiv > 0)).sum() / (populations_by_fractions * (vc_laiv > 0)).sum()

                n_doses_actual = (populations_by_fractions * N) * vc_laiv
                vc_weekly_actual = n_doses_actual / (unvaccinated_fraction_of_SR * (populations_by_fractions * N))

                # do the vaccination with vc_weekly_actual
                frac_to_vaccinate = vc_weekly_actual * init_new[old_state]

                init_new[old_state] = init_new[old_state] - frac_to_vaccinate
                init_new[new_state] += frac_to_vaccinate
                total_doses_frac += frac_to_vaccinate

                ## track doses by week and by age
                my_dose_week = (vacc_week + 1) * 7 + weeks_no_vacc * 7 - 1
                total_laiv_doses_frac_daily[my_dose_week, :] += n_doses_actual

        out, info = odeint(model_equations_seir, init_new.reshape((n_states + 3) * n_ages), tspan[my_t0:my_t0 + 8],
                           args=(parameters_ode,), full_output=True)

        out_main = np.vstack((out_main, out[1:]))

        old_init = out[-1].reshape((n_states + 3, n_ages))
        my_t0 += 7

    total_doses = N * total_doses_frac

    out = odeint(model_equations_seir, old_init.reshape((n_states + 3) * n_ages), tspan[my_t0:],
                 args=(parameters_ode,))

    out_main = np.vstack((out_main, out[1:]))

    return out_main, total_doses, total_iiv_doses_frac_daily, total_laiv_doses_frac_daily


### Runs LAIV 5-12 and LAIV 5-18 scenarios // 9 age groups
def run_ACEFA_scenarios_with_weekly_vacc_3_multirun():
    '''Run primary- and secondary-schoolaged LAIV strategies.'''

    n_ages = 9
    populations_by_fractions = np.array(load_population_australia()['proportion'])
    contact_matrix = load_contact_matrix(populations_by_fractions, n_ages=n_ages)

    baseline_healthseeking = np.array([0.093087, 0.093087, 0.093087, 0.091192, 0.091192,
                                       0.101058, 0.101058, 0.111099, 0.111099])

    (initial_conditions, healthseeking_sets, reproduction_numbers, amplitudes,
     phase_shifts, initial_sw, initial_i) = read_parameters_sensitivity_analysis(baseline_healthseeking)

    n_sims = len(initial_conditions)
    total_weeks = 52  # 364 model days

    results = {name: np.zeros((n_sims, len(POLICIES), len(SCENARIOS), n_ages)) for name in OUTCOMES}

    for sim_index, (initial_population, p_healthseeking, r0, amplitude, phase_shift, sw, i0) in enumerate(zip(
                        initial_conditions, healthseeking_sets, reproduction_numbers,
                        amplitudes, phase_shifts, initial_sw, initial_i), start=1):
        print(f'sim {sim_index}/{n_sims}: '
              f'| R0: {r0:.2f} | amplitude: {amplitude:.3f} '
              f'| phase shift: {phase_shift:.0f}')

        baseline_output, _, baseline_iiv, baseline_laiv = run_model_equations_weekly_vaccination_3_laiv_multirun(
                initial_population=initial_population, amplitude=amplitude, R0=r0,
                vaccine_uptake='pessimistic',
                vaccine_policy='status_quo',
                contact_matrix=contact_matrix)

        baseline_timeseries = baseline_output.reshape((-1, n_states + 3, n_ages))
        hosp_per_infection = get_hospitalisations_per_infection(baseline_timeseries, populations_by_fractions, p_hospitalised, p_asymptomatic)
        death_per_infection = get_deaths_per_infection(baseline_timeseries, populations_by_fractions, p_deaths, p_asymptomatic)

        _, ve_sus, ve_gp, ve_hosp = load_VE_and_VC_IIV(vaccine_policy='status_quo', vaccine_scenario='status_quo')

        parameters_outcome = [total_weeks, N, ve_sus, ve_gp, ve_hosp, p_asymptomatic, p_healthseeking, hosp_per_infection, death_per_infection]

        baseline_outcomes = calculate_outcomes(baseline_timeseries, parameters_outcome)


        for policy_index, policy in enumerate(POLICIES):
            weekly = [np.zeros((len(SCENARIOS), total_weeks)) for _ in OUTCOMES]
            total = [np.zeros((len(SCENARIOS), n_ages)) for _ in OUTCOMES]
            daily = [np.zeros((len(SCENARIOS), 364, n_ages)) for _ in OUTCOMES]
            daily_iiv = np.zeros((len(SCENARIOS), 364, n_ages))
            daily_laiv = np.zeros_like(daily_iiv)

            for outcome_index in range(len(OUTCOMES)):
                weekly[outcome_index][0] = baseline_outcomes['weekly'][outcome_index]
                total[outcome_index][0] = baseline_outcomes['total_age'][outcome_index]
                daily[outcome_index][0] = baseline_outcomes['daily_age'][outcome_index]
            daily_iiv[0] = baseline_iiv
            daily_laiv[0] = baseline_laiv

            for scenario_index, scenario in enumerate(SCENARIOS[1:], start=1):
                out, _, iiv_doses, laiv_doses = run_model_equations_weekly_vaccination_3_laiv_multirun(
                        initial_population=initial_population,
                        amplitude=amplitude, R0=r0,
                        vaccine_uptake=scenario, vaccine_policy=policy,
                        contact_matrix=contact_matrix)
                _, ve_sus, ve_gp, ve_hosp = load_VE_and_VC_LAIV(vaccine_policy=policy, vaccine_scenario=scenario)
                out_main = out.reshape((-1, n_states + 3, n_ages))

                parameters_outcome = [total_weeks, N, ve_sus, ve_gp, ve_hosp, p_asymptomatic, p_healthseeking,
                                      hosp_per_infection, death_per_infection]

                scenario_outcomes = calculate_outcomes(out_main, parameters_outcome)  ## dict

                for outcome_index in range(len(OUTCOMES)):
                    weekly[outcome_index][scenario_index] = scenario_outcomes['weekly'][outcome_index]
                    total[outcome_index][scenario_index] = scenario_outcomes['total_age'][outcome_index]
                    daily[outcome_index][scenario_index] = scenario_outcomes['daily_age'][outcome_index]
                daily_iiv[scenario_index] = iiv_doses
                daily_laiv[scenario_index] = laiv_doses


            for outcome_index, name in enumerate(OUTCOMES):
                results[name][sim_index - 1, policy_index] = total[outcome_index]

    return [results]


### Runs LAIV 2-5 scenarios // 10 age groups
def run_ACEFA_scenarios_preschool_12weeks_multirun():
    '''Run preschool-inclusive LAIV strategies.  '''

    PRESCHOOL_POLICIES = ('laiv_2_4', 'laiv_2_12', 'laiv_2_18')
    PRESCHOOL_SCENARIOS = ('status_quo', 'mid')
    PRESCHOOL_OUTCOMES = ('infections', 'cases', 'hospitalisations', 'deaths')

    age_names = ('0-5M', '6-11M', '1', '2-4', '5-11', '12-17', '18-49', '50-64', '65-79', '80+')
    n_ages = len(age_names)
    total_weeks = 52

    populations_by_fractions = np.asarray(load_population_australia_preschool()['proportion'])

    baseline_healthseeking = np.array([0.093087, 0.093087, 0.093087, 0.093087, 0.091192,
                                       0.091192, 0.101058, 0.101058, 0.111099, 0.111099])

    contact_matrix = load_contact_matrix(populations_by_fractions, n_ages=n_ages)

    (initial_conditions, healthseeking_sets, reproduction_numbers, amplitudes,
     phase_shifts, initial_sw, initial_i) = read_parameters_sensitivity_analysis_preschool(baseline_healthseeking)

    reduced_sus = load_reduced_susceptibility(n_ages=n_ages)
    p_hospitalised = load_hospitalisation_rates_aihw(n_ages=n_ages) / 100_000
    p_deaths = load_death_rates(n_ages=n_ages) / 100_000
    p_asymptomatic = 0.19 * np.ones(n_ages)

    n_sims = len(initial_conditions)
    results = {name: np.zeros((n_sims, len(PRESCHOOL_POLICIES),
                               len(PRESCHOOL_SCENARIOS), n_ages)) for name in PRESCHOOL_OUTCOMES}

    for sim_index, (initial_population, p_healthseeking, r0, amplitude, phase_shift, sw, i0) in enumerate(zip(
                        initial_conditions, healthseeking_sets, reproduction_numbers, amplitudes, phase_shifts,
                        initial_sw, initial_i), start=1):
        print(f'sim {sim_index}/{n_sims}: '
              f'| R0: {r0:.2f} | amplitude: {amplitude:.3f} '
              f'| phase shift: {phase_shift:.0f}')

        baseline_output, _, _, _ = run_model_equations_weekly_vaccination_3_laiv_preschool12_multirun(
                initial_population=initial_population,
                amplitude=amplitude,
                R0=r0,
                vaccine_uptake='mid',
                vaccine_policy='status_quo',
                contact_matrix=contact_matrix)

        baseline_timeseries = baseline_output.reshape((-1, n_states + 3, n_ages))

        hosp_per_infection = get_hospitalisations_per_infection(baseline_timeseries, populations_by_fractions, p_hospitalised, p_asymptomatic)
        death_per_infection = get_deaths_per_infection(baseline_timeseries, populations_by_fractions, p_deaths, p_asymptomatic)

        _, ve_sus, ve_gp, ve_hosp = load_VE_and_VC_IIV2(vaccine_policy='status_quo', vaccine_scenario='status_quo')

        parameters_outcome = [total_weeks, N, ve_sus, ve_gp, ve_hosp, p_asymptomatic,
                              p_healthseeking, hosp_per_infection, death_per_infection]

        baseline_outcomes = calculate_outcomes(baseline_timeseries, parameters_outcome)

        for policy_index, policy in enumerate(PRESCHOOL_POLICIES):
            # Store baseline outcomes
            for outcome_index, name in enumerate(PRESCHOOL_OUTCOMES):
                results[name][sim_index - 1, policy_index, 0] = (baseline_outcomes['total_age'][outcome_index])

            # Run the regular central LAIV scenario (central coverage).
            scenario_output, _, _, _ = run_model_equations_weekly_vaccination_3_laiv_preschool12_multirun(
                    initial_population=initial_population,
                    amplitude=amplitude,
                    R0=r0,
                    vaccine_uptake='mid',
                    vaccine_policy=policy,
                    contact_matrix=contact_matrix)

            scenario_timeseries = scenario_output.reshape((-1, n_states + 3, n_ages))

            # Load central LAIV effectiveness for outcome calculations.
            _, ve_sus, ve_gp, ve_hosp = load_VE_and_VC_LAIV2(vaccine_policy=policy, vaccine_scenario='mid')

            parameters_outcome = [total_weeks, N, ve_sus, ve_gp, ve_hosp, p_asymptomatic,
                                  p_healthseeking, hosp_per_infection, death_per_infection]

            scenario_outcomes = calculate_outcomes(scenario_timeseries, parameters_outcome)

            for outcome_index, name in enumerate(PRESCHOOL_OUTCOMES):
                results[name][sim_index - 1, policy_index, 1] = (scenario_outcomes['total_age'][outcome_index])

    return [results]


if __name__ == "__main__":

    #### Runs LAIV primary and secondary school-aged scenarios // 9 age groups

    ### Runs LAIV 5-12 and LAIV 5-18 scenarios // 9 age groups
    results = run_ACEFA_scenarios_with_weekly_vacc_3_multirun()[0]

    for policy_index, policy in enumerate(POLICIES):
        print(f"Policy: {policy.upper()}")
        for outcome in OUTCOMES:
            print(f"  {outcome.upper()}")
            for scenario_index, scenario in enumerate(SCENARIOS):
                # Cumulative outcome across all ages (40,)
                cumulative_outcomes = results[outcome][:, policy_index, scenario_index, :].sum(axis=1)
                mean_outcome = cumulative_outcomes.mean()
                print(f"    {scenario}: {mean_outcome:,.2f}")



    #### Runs preschool scenarios // 10 age groups

    ## update default params to 10 age groups
    age_names = ('0-5M', '6-11M', '1', '2-4', '5-11', '12-17', '18-49', '50-64', '65-79', '80+')
    n_ages = len(age_names)

    populations_by_fractions = np.asarray(load_population_australia_preschool()['proportion'])

    baseline_healthseeking = np.array([0.093087, 0.093087, 0.093087, 0.093087, 0.091192,
                                       0.091192, 0.101058, 0.101058, 0.111099, 0.111099])

    reduced_sus = load_reduced_susceptibility(n_ages)

    PRESCHOOL_POLICIES = ('laiv_2_4', 'laiv_2_12', 'laiv_2_18')
    PRESCHOOL_SCENARIOS = ('status_quo', 'mid')
    PRESCHOOL_OUTCOMES = ('infections', 'cases', 'hospitalisations', 'deaths')

    ### Runs LAIV 2-5, LAIV 2-12, LAIV 2-18 // 10 age groups
    results = run_ACEFA_scenarios_preschool_12weeks_multirun()[0]



