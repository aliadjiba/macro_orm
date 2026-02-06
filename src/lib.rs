use proc_macro::TokenStream;
use quote::quote;//, format_ident
use syn::{parse_macro_input, ItemStruct, DeriveInput, Meta, Lit, punctuated::Punctuated, Token, Path};//MetaList, token::Comma
use syn::{
    parse::{Parse, ParseStream},
    Ident
};
use convert_case::{Case, Casing};
use pluralizer::pluralize;

use syn::{Data, Fields};

// use syn::{Attribute, Expr};
// use syn::parse::Parser;


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

    // Default table name = struct name lowercase + "s"
    let mut table_name = struct_name.to_string().to_lowercase() + "s";
    let fields = match input.data {
    syn::Data::Struct(data) => data.fields,
        _ => panic!("Model can only be derived for structs"),
    };
    let _id_field = fields.iter().find(|f| {
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

    let expanded = quote! {
        impl orm::model::Model for #struct_name {
            fn table_name() -> &'static str {
                #table_name
            }
            fn id(&self) -> surrealdb::sql::Thing {
                self.id.clone()
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
            "{}_id",
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



/// Custom parser for the macro arguments: RelatedModel, Pivot
struct BelongsToManyArgs {
    related: Path,
    pivot: Path,
}

impl Parse for BelongsToManyArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let args: Punctuated<Path, Token![,]> = Punctuated::parse_terminated(input)?;
        if args.len() != 2 {
            return Err(input.error("Expected exactly two types: RelatedModel, Pivot"));
        }
        let mut iter = args.into_iter();
        let related = iter.next().unwrap();
        let pivot = iter.next().unwrap();
        Ok(Self { related, pivot })
    }
}

#[proc_macro_attribute]
pub fn belongs_to_many(attr: TokenStream, item: TokenStream) -> TokenStream {
    // Parse the struct
    let input = parse_macro_input!(item as ItemStruct);

    // Parse the macro arguments
    let args = parse_macro_input!(attr as BelongsToManyArgs);
    let related_path = args.related;
    let pivot_path = args.pivot;
    let struct_name = &input.ident;

    // Generate the function name (plural snake_case)
    let fn_name = relation_plural_fn_name_from_path(&related_path, struct_name.span());

    let expanded = quote! {
        #input

        impl #struct_name {
            pub fn #fn_name<'a>(&self, repo: &'a orm::repository::Repo) -> orm::model::BelongsToMany<'a, #pivot_path> {
                use orm::model::Pivot;

                // Automatically determine which side is left
                let is_left = {
                    let left_key = #pivot_path::left_key();
                    // Compare with table name
                    left_key == "id" || left_key.to_lowercase().contains(&Self::table_name().to_lowercase())
                };

                orm::model::BelongsToMany::new(repo, self.id.clone(), is_left)
            }
        }
    };

    TokenStream::from(expanded)
}


#[proc_macro_derive(PivotModel, attributes(left, right))]
pub fn pivot_model_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = input.ident;

    let fields = match input.data {
        Data::Struct(ref data) => match &data.fields {
            Fields::Named(named) => named.named.iter().collect::<Vec<_>>(),
            _ => panic!("PivotModel requires named fields"),
        },
        _ => panic!("PivotModel can only be derived for structs"),
    };

    let mut left_field = None;
    let mut right_field = None;

    for field in fields.iter() {
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

    let table_name = struct_name
        .to_string()
        .replace("Pivot", "")
        .to_lowercase();

    let expanded = quote! {

        impl orm::model::Model for #struct_name {
            fn table_name() -> &'static str {
                #table_name
            }

            fn id(&self) -> surrealdb::sql::Thing {
                self.id.clone()
            }
        }

        impl orm::model::Pivot for #struct_name {

            fn left_key() -> &'static str {
                stringify!(#left_ident)
            }

            fn right_key() -> &'static str {
                stringify!(#right_ident)
            }

            fn left_id(&self) -> &surrealdb::sql::Thing {
                &self.#left_ident
            }

            fn right_id(&self) -> &surrealdb::sql::Thing {
                &self.#right_ident
            }

            fn new(
                left: surrealdb::sql::Thing,
                right: surrealdb::sql::Thing
            ) -> Self {

                let now = chrono::Utc::now().to_rfc3339();

                Self {
                    id: surrealdb::sql::Thing::from((
                        Self::table_name().to_string(),
                        surrealdb::Uuid::new_v4().to_string()
                    )),
                    #left_ident: left,
                    #right_ident: right,
                    created_at: now.clone(),
                    updated_at: now,
                }
            }
        }
    };

    TokenStream::from(expanded)
}



