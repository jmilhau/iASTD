type state_to_modify=ASTD_state.t
type modified_state=ASTD_state.t

type astd_name = string
type called_path = astd_name list


val execute: ASTD_state.t -> ASTD_astd.t -> ASTD_event.t -> astd_name list ->ASTD_environment.t -> called_path
						-> (ASTD_state.t * astd_name list * ASTD_state.t *(((astd_name*ASTD_constant.t*ASTD_environment.t*called_path)*ASTD_state.t*bool) list)* (((ASTD_optimisation.dependency*(ASTD_term.t list))*ASTD_term.t*bool) list) *bool)


val execute_event_list: int->ASTD_state.t -> ASTD_astd.t -> ASTD_event.t list -> ASTD_state.t
