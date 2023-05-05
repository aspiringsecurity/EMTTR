use cosmwasm_std::StdError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ContractError {
    #[error("{0}")]
    Std(#[from] StdError),

    #[error("Unauthorized")]
    Unauthorized {},

    #[error("Use has existing escrow")]
    ExistingEscrow {},

    #[error("Use has no existing escrow")]
    NoExistingEscrow {},

    #[error("Escrow has not expired")]
    NotExpired {},
    // Add any other custom errors you like here.
    // Look at https://docs.rs/thiserror/1.0.21/thiserror/ for details.
}
