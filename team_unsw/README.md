# ACEFA Scenario Modelling Hub (Team UNSW): LAIV scenario modelling in Australia

This repository contains Python code for an age-structured, deterministic influenza transmission model used to 
evaluate live-attenuated influenza vaccine (LAIV) strategies alongside existing inactivated influenza vaccine (IIV) 
vaccination in Australia.

The analysis considers LAIV programmes targeting primary school-aged children, primary and secondary school-aged 
children, and strategies that additionally include preschool-aged children. Outcomes are estimated over a 52-week 
(364-day) period across 40 calibrated parameter sets.

## Vaccination strategies

| Model | Strategy identifier | Target age groups | Scenarios run by the current workflow |
| --- | --- | --- | --- |
| 9 age groups | `laiv_5_12` | 5–11 years | Pessimistic, central (`mid`), optimistic |
| 9 age groups | `laiv_5_18` | 5–17 years | Pessimistic, central (`mid`), optimistic |
| 10 age groups | `laiv_2_4` | 2–4 years | Central (`mid`) |
| 10 age groups | `laiv_2_12` | 2–11 years | Central (`mid`) |
| 10 age groups | `laiv_2_18` | 2–17 years | Central (`mid`) |

Each strategy is compared with a status quo IIV-only programme. 


## Repository structure

```text
.
├── main_team_unsw.py            # simulation workflow
├── acefa_functions_team_unsw.py       # Model equations, vaccination rollout schedules, outcome functions
├── default_params_team_unsw.py        # Model parameters, population, vaccine assumptions
├── data/
│   ├── optimization_results_p_hs_scale_wo_a_ps_365.csv
│   ├── population_australia_2024_abs_australia.xlsx
│   ├── ACEFA_UK_POLYMOD_contact_matrix.csv
│   └── ACEFA_UK_POLYMOD_contact_matrix_laiv_2_4.csv
└── README.md
```

## Model and outcomes

The model is an age-structured SEIR-type model with unvaccinated, vaccinated and pre-existing-immunity states. 

The scenario-running functions return a **one-element list containing a dictionary**. Each dictionary has 
four outcome keys: **`infections, cases, hospitalisations, deaths`**.

For the 9-age-group model, each outcome array has shape:

```text
(n_simulations, 2 policies, 4 scenarios, 9 age groups)
```

The policy order is `laiv_5_12`, `laiv_5_18`. 

The scenario order is `status_quo`, `pessimistic`, `mid`, `optimistic`.

For the 10-age-group (preschool-inclusive) model, each outcome array has shape:

```text
(n_simulations, 3 policies, 2 scenarios, 10 age groups)
```

The policy order is `laiv_2_4`, `laiv_2_12`, `laiv_2_18`. 

The scenario order is `status_quo`, `mid`.
