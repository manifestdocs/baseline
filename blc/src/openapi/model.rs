use std::collections::HashMap;

/// Intermediate representation for an API extracted from Baseline source code.
/// This is the shared form consumed by OpenAPI serialization, contract testing, etc.
pub struct ApiModel {
    pub routes: Vec<ApiRoute>,
    pub schemas: HashMap<String, ApiSchema>,
}

pub struct ApiRoute {
    pub method: String,
    /// OpenAPI format path, e.g. /users/{id}
    pub path: String,
    /// Handler function name, used as operationId
    pub operation_id: String,
    pub summary: Option<String>,
    /// Schema ref name from Request.decode / Request.body_json
    pub request_body: Option<String>,
    pub parameters: Vec<ApiParameter>,
    pub responses: Vec<ApiResponse>,
}

pub struct ApiParameter {
    pub name: String,
    pub location: ParamLocation,
    pub param_type: ApiType,
    pub required: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParamLocation {
    Path,
    Query,
    Header,
}

pub struct ApiResponse {
    pub status: u16,
    pub description: String,
    pub schema: Option<ApiSchemaRef>,
}

#[derive(Debug, Clone)]
pub enum ApiSchemaRef {
    Ref(String),
    Inline(ApiType),
}

pub struct ApiSchema {
    pub schema_type: ApiSchemaType,
}

pub enum ApiSchemaType {
    Object { fields: Vec<ApiField> },
    Enum(Vec<String>),
    Alias(ApiType, Option<ApiConstraint>),
}

pub struct ApiField {
    pub name: String,
    pub field_type: ApiType,
    pub required: bool,
    pub constraints: Option<ApiConstraint>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ApiType {
    String,
    Int,
    Float,
    Bool,
    List(Box<ApiType>),
    Ref(String),
    Any,
}

#[derive(Debug, Clone)]
pub enum ApiConstraint {
    IntRange {
        min: Option<i64>,
        max: Option<i64>,
    },
    StringPattern(String),
    StringLength {
        min: Option<usize>,
        max: Option<usize>,
    },
    StringEnum(Vec<String>),
    All(Vec<ApiConstraint>),
}
