defmodule REST.Mixfile do
  use Mix.Project

  def project do
    [app: :rest,
     version: "1.5.0",
     description: "REST erlang interface generator",
     package: package]
  end

  defp package do
    [files: ~w(src LICENSE mix.exs README.md rebar.config),
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/synrc/rest"}]
   end
end
