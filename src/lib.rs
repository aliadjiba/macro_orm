use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Meta, Lit};

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
        impl Model for #struct_name {
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
    let parent = parse_macro_input!(attr as syn::Path);

    let struct_name = &input.ident;
    let parent_name = &parent;

    let fn_name = syn::Ident::new(
        &parent_name.segments.last().unwrap().ident.to_string().to_lowercase(),
        struct_name.span(),
    );

    let expanded = quote! {
        #input

        impl #struct_name {
            pub fn #fn_name<'a>(&self, repo: &'a Repo) -> Query<'a, #parent_name> {
                Query::<#parent_name>::new(repo)
                    .where_eq("id", self.user_id.clone())
            }
        }
    };

    TokenStream::from(expanded)
}
