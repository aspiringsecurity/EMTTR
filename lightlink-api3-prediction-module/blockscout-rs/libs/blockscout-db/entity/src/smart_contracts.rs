//! `SeaORM` Entity. Generated by sea-orm-codegen 0.12.6

use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel, Eq)]
#[sea_orm(table_name = "smart_contracts")]
pub struct Model {
    #[sea_orm(primary_key)]
    pub id: i64,
    pub name: String,
    pub compiler_version: String,
    pub optimization: bool,
    #[sea_orm(column_type = "Text")]
    pub contract_source_code: String,
    #[sea_orm(column_type = "JsonBinary")]
    pub abi: Json,
    #[sea_orm(column_type = "Binary(BlobSize::Blob(None))")]
    pub address_hash: Vec<u8>,
    pub inserted_at: DateTime,
    pub updated_at: DateTime,
    #[sea_orm(column_type = "Text", nullable)]
    pub constructor_arguments: Option<String>,
    pub optimization_runs: Option<i64>,
    pub evm_version: Option<String>,
    pub external_libraries: Option<Vec<Json>>,
    pub verified_via_sourcify: Option<bool>,
    pub is_vyper_contract: Option<bool>,
    pub partially_verified: Option<bool>,
    #[sea_orm(column_type = "Text", nullable)]
    pub file_path: Option<String>,
    pub is_changed_bytecode: Option<bool>,
    pub bytecode_checked_at: Option<DateTime>,
    pub contract_code_md5: String,
    pub implementation_name: Option<String>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(
        belongs_to = "super::addresses::Entity",
        from = "Column::AddressHash",
        to = "super::addresses::Column::Hash",
        on_update = "NoAction",
        on_delete = "Cascade"
    )]
    Addresses,
    #[sea_orm(has_many = "super::smart_contracts_additional_sources::Entity")]
    SmartContractsAdditionalSources,
}

impl Related<super::addresses::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::Addresses.def()
    }
}

impl Related<super::smart_contracts_additional_sources::Entity> for Entity {
    fn to() -> RelationDef {
        Relation::SmartContractsAdditionalSources.def()
    }
}

impl ActiveModelBehavior for ActiveModel {}