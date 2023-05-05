use cosmwasm_std::{Addr, Deps, StdError, StdResult};

use crate::msg::{ConfigResponse, EscrowResponse};
use crate::state::{CONFIG, ESCROW};

pub fn query_config(deps: Deps) -> StdResult<ConfigResponse> {
    let config = CONFIG.load(deps.storage)?;
    Ok(ConfigResponse {
        owner: config.owner.to_string(),
        token: config.token.to_string(),
    })
}

pub fn query_escrow(deps: Deps, user: Addr) -> StdResult<EscrowResponse> {
    let escrow = ESCROW.may_load(deps.storage, &user)?;

    if escrow.is_none() {
        return Err(StdError::generic_err("No escrow found"));
    }

    let escrow = escrow.unwrap();

    Ok(EscrowResponse {
        amount: escrow.amount,
        time: escrow.time,
    })
}
