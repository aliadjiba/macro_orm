use proc_macro::TokenStream;
use quote::{quote};
use syn::{parse_macro_input, ItemStruct, DeriveInput, Meta, Lit, punctuated::Punctuated, Token, Path};//MetaList, token::Comma
use syn::{
    parse::{Parse, ParseStream},
    Ident
};
use syn::{
    Type, PathArguments, GenericArgument,
};
use convert_case::{Case, Casing};
use pluralizer::pluralize;

use syn::{Data, Fields};

// use syn::{Attribute, Expr};
// use syn::parse::Parser;


fn rust_type_to_surreal(field_name: &str, ty: &Type) -> String {
    match ty {
        Type::Path(type_path) => {
            let segment =
                &type_path.path.segments.last().unwrap();

            let ident =
                segment.ident.to_string();

            match ident.as_str() {
                "String" => "string".to_string(),
                "bool" => "bool".to_string(),

                "i8" | "i16" | "i32" | "i64" |
                "u8" | "u16" | "u32" | "u64" |
                "usize" | "isize" => "int".to_string(),

                "f32" | "f64" => "float".to_string(),

                "RecordId" => {
                    if field_name == "id" {
                        "record".to_string()
                    } else {
                        format!("record<{}>", field_name)
                    }
                },

                "Option" => {
                    if let PathArguments::AngleBracketed(args) =
                        &segment.arguments
                    {
                        if let Some(GenericArgument::Type(inner_ty)) =
                            args.args.first()
                        {
                            let inner =
                                rust_type_to_surreal(field_name, inner_ty);

                            return format!(
                                "option<{}>",
                                inner
                            );
                        }
                    }

                    "option<any>".to_string()
                }

                "Vec" => {
                    if let PathArguments::AngleBracketed(args) =
                        &segment.arguments
                    {
                        if let Some(GenericArgument::Type(inner_ty)) =
                            args.args.first()
                        {
                            let inner =
                                rust_type_to_surreal(field_name, inner_ty);

                            return format!(
                                "array<{}>",
                                inner
                            );
                        }
                    }

                    "array".to_string()
                }

                _ => "any".to_string(),
            }
        }

        _ => "any".to_string(),
    }
}


/// Helper to generate plural snake_case method names
fn relation_plural_fn_name_from_path(path: &Path, span: proc_macro2::Span) -> Ident {
    let segment = path
        .segments
        .last()
        .expect("Expected a valid type path for relation");

    let snake = segment.ident.to_string().to_case(Case::Snake);
    let plural = pluralize(&snake, 2, false);

    Ident::new(&plural, span)
}


#[proc_macro_derive(Model, attributes(table))]
pub fn derive_model(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_name = input.ident;

    let mut table_name = struct_name.to_string().to_lowercase();

    let fields = match input.data {
        syn::Data::Struct(data) => data.fields,
        _ => panic!("Model can only be derived for structs"),
    };

    fields.iter().find(|f| {
        f.ident.as_ref().unwrap() == "id"
    }).expect("Model requires an `id` field");

    for attr in input.attrs {
        if attr.path().is_ident("table") {
            if let Meta::NameValue(meta) = attr.meta {
                if let syn::Expr::Lit(expr_lit) = meta.value {
                    if let Lit::Str(lit_str) = expr_lit.lit {
                        table_name = lit_str.value();
                    }
                }
            }
        }
    }

    let mut migration_sql = format!(
        "DEFINE TABLE IF NOT EXISTS {} SCHEMAFULL;\n",
        table_name
    );

    for field in fields.iter() {
        let ident = field.ident.as_ref().unwrap();

        if ident == "id" {
            continue;
        }

        let field_name = ident.to_string();

        let surreal_type =
            rust_type_to_surreal(&field_name, &field.ty);

        migration_sql.push_str(
            &format!(
                "DEFINE FIELD IF NOT EXISTS {} ON {} TYPE {};\n",
                field_name,
                table_name,
                surreal_type
            )
        );
    }

    let expanded = quote! {
        impl orm::model::Model for #struct_name {
            fn table_name() -> String {
                #table_name.to_string()
            }

            fn id(&self) -> surrealdb::types::RecordId {
                self.id.clone()
            }

            fn migration() -> &'static str {
                #migration_sql
            }
        }
    };

    TokenStream::from(expanded)
}




#[proc_macro_attribute]
pub fn belongs_to(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as syn::ItemStruct);
    let parent_name = parse_macro_input!(attr as syn::Path);

    let struct_name = &input.ident;

    // Extract parent type ident safely
    let parent_segment = parent_name
        .segments
        .last()
        .expect("Expected a valid parent type path");

    let parent_ident = &parent_segment.ident;

    // Function name (snake_case, singular for belongs_to)
    use convert_case::{Case, Casing};

    let fn_name = syn::Ident::new(
        &parent_ident.to_string().to_case(Case::Snake),
        struct_name.span(),
    );

    // Foreign key name: user_id, lab_id, etc.
    let fk_field = syn::Ident::new(
        &format!(
            "{}",
            parent_ident.to_string().to_case(Case::Snake)
        ),
        struct_name.span(),
    );


    let expanded = quote! {
        #input

        impl #struct_name {
            pub fn #fn_name<'a>(
                &self,
                repo: &'a orm::repository::Repo
            ) -> orm::model::BelongsTo<'a, #struct_name, #parent_name> {
                orm::model::BelongsTo::new(
                    repo,
                    stringify!(#fk_field),
                    self.#fk_field.clone(),
                )
            }
        }
    };

    TokenStream::from(expanded)
}


#[proc_macro_attribute]
pub fn has_many(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as syn::ItemStruct);
    let child_path = parse_macro_input!(attr as syn::Path);

    let struct_name = &input.ident;

    // Extract child ident safely
    let child_segment = child_path
        .segments
        .last()
        .expect("Expected a valid child type path");

    use convert_case::{Case, Casing};
    use pluralizer::pluralize;

    // PascalCase → snake_case
    let snake = child_segment.ident.to_string().to_case(Case::Snake);

    // Proper pluralization
    let plural = pluralize(&snake, 2, false);

    // Create function name
    let fn_name = syn::Ident::new(&plural, struct_name.span());

    let expanded = quote! {
        #input

        impl #struct_name {
            pub fn #fn_name<'a>(
                &self,
                repo: &'a orm::repository::Repo
            ) -> orm::model::HasMany<'a, #struct_name, #child_path> {
                orm::model::HasMany::new(repo)
            }
        }
    };

    TokenStream::from(expanded)
}



struct BelongsToManyArgs {
    related: Path,
    pivot: Path,
    side: Ident,
}

impl Parse for BelongsToManyArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let args: Punctuated<syn::Expr, Token![,]> =
            Punctuated::parse_terminated(input)?;

        if args.len() != 3 {
            return Err(input.error(
                "Expected: RelatedModel, PivotModel, left|right"
            ));
        }

        let mut iter = args.into_iter();

        let related = match iter.next().unwrap() {
            syn::Expr::Path(p) => p.path,
            _ => return Err(input.error("Expected model type")),
        };

        let pivot = match iter.next().unwrap() {
            syn::Expr::Path(p) => p.path,
            _ => return Err(input.error("Expected pivot type")),
        };

        let side = match iter.next().unwrap() {
            syn::Expr::Path(p) => {
                p.path.get_ident()
                    .cloned()
                    .ok_or_else(|| input.error("Expected left or right"))?
            }
            _ => return Err(input.error("Expected left or right")),
        };

        if side != "left" && side != "right" {
            return Err(input.error("Side must be `left` or `right`"));
        }

        Ok(Self { related, pivot, side })
    }
}

#[proc_macro_attribute]
pub fn belongs_to_many(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemStruct);
    let args = parse_macro_input!(attr as BelongsToManyArgs);

    let struct_name = &input.ident;
    let related_path = args.related;
    let pivot_path = args.pivot;
    let side = args.side;

    let fn_name =
        relation_plural_fn_name_from_path(&related_path, struct_name.span());

    let is_left = if side == "left" {
        quote! { true }
    } else {
        quote! { false }
    };

    let expanded = quote! {
        #input

        impl #struct_name
        where
            Self: orm::model::Model,
        {
            pub fn #fn_name<'a>(
                &self,
                repo: &'a orm::repository::Repo,
            ) -> orm::model::BelongsToMany<'a, #pivot_path>
            {
                orm::model::BelongsToMany::new(
                    repo,
                    self.id.clone(),
                    #is_left
                )
            }
        }
    };

    TokenStream::from(expanded)
}



#[proc_macro_derive(PivotModel, attributes(left, right, timestamp))]
pub fn pivot_model_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = input.ident.clone();
    let mut table_name = struct_name.to_string().to_lowercase();
    // 1️⃣ Collect fields
    let fields = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(named) => named.named.iter().collect::<Vec<_>>(),
            _ => panic!("PivotModel requires named fields"),
        },
        _ => panic!("PivotModel can only be derived for structs"),
    };

    // 2️⃣ Detect left and right
    let mut left_field = None;
    let mut right_field = None;

    for field in &fields {
        for attr in &field.attrs {
            if attr.path().is_ident("left") {
                left_field = Some(field.ident.clone().unwrap());
            }
            if attr.path().is_ident("right") {
                right_field = Some(field.ident.clone().unwrap());
            }
        }
    }

    let left_ident = left_field.expect("Missing #[left] field");
    let right_ident = right_field.expect("Missing #[right] field");

    // 3️⃣ Detect timestamp attribute
    let has_timestamp = input.attrs.iter().any(|attr| attr.path().is_ident("timestamp"));

    // 4️⃣ Collect extra fields (exclude id, left, right, timestamps)
    let extra_fields: Vec<(syn::Ident, &syn::Type)> = fields
        .iter()
        .filter_map(|f| {
            let ident = f.ident.clone().unwrap();
            let ty = &f.ty;
            if ident != left_ident
                && ident != right_ident
                && ident != "id"
                && !(has_timestamp && (ident == "created_at" || ident == "updated_at"))
            {
                Some((ident, ty))
            } else {
                None
            }
        })
        .collect();

    // 5️⃣ Generate Extra tuple type
    let extra_type = if extra_fields.is_empty() {
        quote! { () }
    } else {
        let types = extra_fields.iter().map(|(_, ty)| quote! { #ty });
        quote! { ( #( #types ),* ) }
    };

    // 6️⃣ Generate tuple destructuring
    let extra_destructure = if extra_fields.is_empty() {
        quote! {}
    } else {
        let idents = extra_fields.iter().map(|(ident, _)| ident);
        quote! {
            let ( #( #idents ),* ) = extra;
        }
    };

    // 7️⃣ Generate assignments for Self { ... }
    let extra_assignments = extra_fields.iter().map(|(ident, _)| quote! { #ident: #ident });

    // 8️⃣ Timestamp initialization
    let timestamp_init = if has_timestamp {
        quote! { let now = chrono::Utc::now().to_rfc3339(); }
    } else {
        quote! {}
    };

    let timestamp_assign = if has_timestamp {
        quote! {
            created_at: now.clone(),
            updated_at: now,
        }
    } else {
        quote! {}
    };
    
    for attr in &input.attrs {
    if attr.path().is_ident("table") {
        if let Meta::NameValue(meta) = &attr.meta {
            if let syn::Expr::Lit(expr_lit) = &meta.value {
                if let Lit::Str(lit_str) = &expr_lit.lit {
                    table_name = lit_str.value();
                }
            }
        }
    }
}

    let mut migration_sql = format!(
        "DEFINE TABLE IF NOT EXISTS {} SCHEMAFULL;\n",
        table_name
    );

    for field in &fields {
        let ident = field.ident.as_ref().unwrap();

        if ident == "id" {
            continue;
        }

        let field_name = ident.to_string();

        let surreal_type =
            rust_type_to_surreal(&field_name ,&field.ty);

        migration_sql.push_str(
            &format!(
                "DEFINE FIELD IF NOT EXISTS {} ON {} TYPE {};\n",
                field_name,
                table_name,
                surreal_type
            )
        );
    }

    let migration_literal =
        syn::LitStr::new(
            &migration_sql,
            proc_macro2::Span::call_site()
        );

    let table_literal =
        syn::LitStr::new(
            &table_name,
            proc_macro2::Span::call_site()
        );

    // 9️⃣ Generate final macro output
    let expanded = quote! {

        impl orm::model::Model for #struct_name {
            // ✅ Return String instead of &str to avoid temporary reference
            fn table_name() -> String {
                #table_literal.to_string()
            }

            fn id(&self) -> surrealdb::types::RecordId {
                self.id.clone()
            }
            fn migration() -> &'static str {
                #migration_literal
            }
        }

        impl orm::model::Pivot for #struct_name {

            type Extra = #extra_type;

            fn left_key() -> &'static str {
                stringify!(#left_ident)
            }

            fn right_key() -> &'static str {
                stringify!(#right_ident)
            }

            fn left_id(&self) -> &surrealdb::types::RecordId {
                &self.#left_ident
            }

            fn right_id(&self) -> &surrealdb::types::RecordId {
                &self.#right_ident
            }

            /// Constructor with extra fields
            fn new_with(left: surrealdb::types::RecordId, right: surrealdb::types::RecordId, extra: Self::Extra) -> Self {
                #extra_destructure
                #timestamp_init

                Self {
                    id: surrealdb::types::RecordId{
                        table: Self::table_name().into(),
                        key: surrealdb::types::RecordIdKey::String(surrealdb::types::Uuid::new_v4().to_string())
                    },
                    #left_ident: left,
                    #right_ident: right,
                    #( #extra_assignments, )*
                    #timestamp_assign
                }
            }

            /// Convenience constructor for builder/old code
            fn new(left: surrealdb::types::RecordId, right: surrealdb::types::RecordId) -> Self {
                Self::new_with(left, right, Default::default())
            }
        }
    };

    TokenStream::from(expanded)
}
