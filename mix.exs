defmodule REST.Mixfile do
  use Mix.Project

  def project do
    [app: :rest,
     version: "6.11.1",
     description: "REST erlang interface generator",
     deps: deps(),
     package: package()]
  end

  def application() do
    [
      mod: {:rest, []},
      applications: [:public_key,:asn1,:kernel,:stdlib,:ranch,:cowboy,:syntax_tools,:compiler,:rocksdb,:kvs, :erp, :bpe]
    ]
  end

  def deps, do: [ {:ex_doc, "~> 0.11", only: :dev},
                  {:rocksdb, "~> 1.6.0"},
                  {:bpe, "~> 6.11.0"},
                  {:erp, "~> 1.11.0"},
                  {:jsone, "~> 1.5.0"},
                  {:cowboy, "~> 2.5.0"} ]

  defp package do
    [files: ~w(src LICENSE mix.exs README.md rebar.config),
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/synrc/rest"}]
   end
end
