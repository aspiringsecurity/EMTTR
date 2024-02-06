use blockscout_service_launcher::{
    launcher::{ConfigSettings, MetricsSettings, ServerSettings},
    tracing::{JaegerSettings, TracingSettings},
};
use serde::Deserialize;

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct Settings {
    #[serde(default)]
    pub server: ServerSettings,
    #[serde(default)]
    pub metrics: MetricsSettings,
    #[serde(default)]
    pub tracing: TracingSettings,
    #[serde(default)]
    pub jaeger: JaegerSettings,
    {% if database -%}
    pub database: DatabaseSettings,
    {% endif -%}

}

impl ConfigSettings for Settings {
    const SERVICE_NAME: &'static str = "{{PROJECT_NAME}}";
}
{% if database %}
#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct DatabaseSettings {
    pub url: String,
    #[serde(default)]
    pub create_database: bool,
    #[serde(default)]
    pub run_migrations: bool,
}
{% endif %}
impl Settings {
    pub fn default(
        {% if database %}
        database_url: String,
        {% endif %}
    ) -> Self {
        Self {
            server: Default::default(),
            metrics: Default::default(),
            tracing: Default::default(),
            jaeger: Default::default(),
            {% if database -%}
            database: DatabaseSettings {
                url: database_url,
                create_database: false,
                run_migrations: false,
            },{% endif %}
        }
    }
}
