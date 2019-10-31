defmodule REST.Mixfile do
  use Mix.Project

  def project do
    [app: :rest,
     version: "5.10.8",
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
                  {:rocksdb, "~> 1.3.2"},
                  {:erp, "~> 0.10.4"},
                  {:bpe, "~> 4.10.24"},
                  {:jsone, "~> 1.5.0"},
                  {:cowboy, "~> 2.5.0"} ]

  defp package do
    [files: ~w(src LICENSE mix.exs README.md rebar.config),
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/synrc/rest"}]
   end
end
