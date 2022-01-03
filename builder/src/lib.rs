use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, Data, DataStruct, DeriveInput, Field,
    Fields, FieldsNamed,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { named, .. }),
        ..
    }) = &input.data
    {
        let origin_impl = origin_impl(&input, named);
        let builder_struct = builder_struct(&input, named);
        let builder_impl = builder_impl(&input, named);

        quote! {
            #origin_impl
            #builder_struct
            #builder_impl
        }
        .into()
    } else {
        unimplemented!()
    }
}

fn origin_impl(input: &DeriveInput, named: &Punctuated<Field, Comma>) -> impl ToTokens {
    let origin_struct_ident = &input.ident;
    let builder_struct_ident = format_ident!("{}Builder", origin_struct_ident);

    let builder_struct_fields_default = named.iter().map(|field| {
        let field_ident = &field.ident;
        quote!(#field_ident: std::option::Option::None)
    });
    quote! {
        impl #origin_struct_ident {
            pub fn builder() -> #builder_struct_ident {
                #builder_struct_ident {
                    #(#builder_struct_fields_default),*
                }
            }
        }
    }
}

fn builder_struct(input: &DeriveInput, named: &Punctuated<Field, Comma>) -> impl ToTokens {
    let origin_struct_ident = &input.ident;
    let builder_struct_ident = format_ident!("{}Builder", origin_struct_ident);

    let builder_struct_fields = named.iter().map(|field| {
        let field_ident = &field.ident;
        let field_ty = &field.ty;
        quote!(#field_ident: std::option::Option<#field_ty>)
    });
    quote! {
        pub struct #builder_struct_ident {
            #(#builder_struct_fields),*
        }
    }
}

fn builder_impl(input: &DeriveInput, named: &Punctuated<Field, Comma>) -> impl ToTokens {
    let origin_struct_ident = &input.ident;
    let builder_struct_ident = format_ident!("{}Builder", origin_struct_ident);

    let builder_fields = named.iter().map(|field| {
        let field_ident = &field.ident;
        quote!(#field_ident: self.#field_ident.clone().ok_or(concat!(stringify!(#field_ident), " has not been set yet"))?)
    });
    let builder_setters = named.iter().map(|field| {
        let field_ident = &field.ident;
        let field_ty = &field.ty;
        quote! {
            pub fn #field_ident(&mut self, #field_ident: #field_ty) -> &mut Self {
                self.#field_ident = Some(#field_ident);
                self
            }
        }
    });
    quote! {
        impl #builder_struct_ident {
            pub fn build(&mut self) -> Result<#origin_struct_ident, Box<dyn std::error::Error>> {
                std::result::Result::Ok(
                    #origin_struct_ident {
                        #(#builder_fields),*
                    }
                )
            }
            #(#builder_setters)*
        }
    }
}
